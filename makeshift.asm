#import "common.inc"
#import "basic.inc"

    .const uncompressed_start_addr = basic_reentry

    .pc = start_addr "autostart"
autostart:
line_10:
    // Call into entry subroutine
    StartBasicLine(line_20, 10, basic_token_sys)
    .text "2061"
    EndBasicLine()
line_20:
    EndBasicProgram()

    .const reloc_addr = $8000

    .pc = * "stub entry"
entry:
    // Disable interrupts
    //  Technically we should be enabling them again after the decompression routine,
    //  but since we know we'll jump back into code that'll disable them anyways, it
    //  should be fine.
    sei

    // Copy 8kb block starting at the packed data to $8000-$9fff in 32 256-byte chunks
    ldy #$00
    ldx #$20
reloc_block_loop:
reloc_block_load_instr:
!:          lda decomp_start, y
reloc_block_store_instr:
            sta reloc_addr, y
        iny
        bne !-

        inc reloc_block_load_instr + 2
        inc reloc_block_store_instr + 2
    dex
    bne reloc_block_loop

    jmp decomp_entry

    .var uncompressed_data = LoadBinary("intro.bin");
    .var uncompressed_intro_end = uncompressed_start_addr + uncompressed_data.getSize()

    // Available zp addrs:
    //  $02 (unused)
    //  $92-97 (only used during either rs232 or datasette io)
    //  $a3-$b1 (only used during either rs232 or datasette io)
    //  $f7-$fa (only used during rs232 io)
    //  $fb-$fe (unused)
    //  $ff (only used during fp -> string conversion)

    .const table_base = $c000

    .const table_offset_low = table_base
    .const table_offset_high = table_base + $30
    .const table_additional_bits = table_base + $60

    .const length_table_offset_low = table_offset_low
    .const length_table_offset_high = table_offset_high
    .const length_table_additional_bits = table_additional_bits

    .const distance_table_offset_low = table_offset_low + $10
    .const distance_table_offset_high = table_offset_high + $10
    .const distance_table_additional_bits = table_additional_bits + $10

    .const table_offset_low_temp = $92
    .const table_offset_high_temp = $93
    .const table_offset_add_low_temp = $94
    .const table_offset_add_high_temp = $95

    .const bit_buffer = $fb
    .const bit_index = $fc

    .const backref_length_low = $92
    .const backref_length_high = $93

    // These addr's have to be consecutive
    .const backref_ptr_low = $fd
    .const backref_ptr_high = $fe

    .const read_bit_temp = $02

    .const read_n_bit_value_low = $a3
    .const read_n_bit_value_high = $a4

    .pc = * "decompressor and packed intro"
decomp_start:
    .pseudopc reloc_addr {
decomp_entry:
        // Expects y to be 0 on entry
        sty bit_buffer
        sty bit_index

        // Decode table entries
decomp_decode_table:
            // Reset current offset add amount
            ldx #$00
            stx table_offset_add_high_temp
            inx
            stx table_offset_add_low_temp

            // if (i & 0x0f) == 0 then reset current offset to 1
            tya
            and #$0f
            bne !+
                sta table_offset_high_temp
                txa
                sta table_offset_low_temp

            // Read additional bit count
!:          lda #$04
            jsr decomp_read_n_bit_value

            // Store additional bit count into table
            sta table_additional_bits, y

            // Shift current offset add amount (should be 1) by number of additional bits
            tax
            beq decomp_decode_table_store_and_add
!:              asl table_offset_add_low_temp
                rol table_offset_add_high_temp
            dex
            bne !-

            // Store current offset into tables, and add shifted amount
decomp_decode_table_store_and_add:
            lda table_offset_low_temp
            sta table_offset_low, y
            clc
            adc table_offset_add_low_temp
            sta table_offset_low_temp
            lda table_offset_high_temp
            sta table_offset_high, y
            adc table_offset_add_high_temp
            sta table_offset_high_temp
        iny
        cpy #$30
        bne decomp_decode_table

        // Reset y, as the rest of the decompressor assumes its value is always 0
        ldy #$00

decomp_packet:
            // Read packet bit
            jsr decomp_read_bit

            beq decomp_literal
                // Backreference

                // Read length
                //  Read length index
                jsr decomp_read_unary
                //  Read additional bits
                pha
                lda length_table_additional_bits, x
                jsr decomp_read_n_bit_value
                //  Add offset to additional bits, and store in backref_length
                pla
                tax
                lda length_table_offset_low, x
                clc
                adc read_n_bit_value_low
                sta backref_length_low
                lda length_table_offset_high, x
                adc read_n_bit_value_high
                sta backref_length_high

                // Read distance
                //  Read distance index
                jsr decomp_read_unary
                //  if length == 1 add 16 to distance index
                cpy backref_length_high
                bne !+
                ldx backref_length_low
                dex
                bne !+
                    clc
                    adc #$10
                //  Subtract table offset from current output pointer and store into backref_ptr
!:              tax
                lda decomp_write_instr + 1
                sec
                sbc distance_table_offset_low, x
                sta backref_ptr_low
                lda decomp_write_instr + 2
                sbc distance_table_offset_high, x
                sta backref_ptr_high

                //  Read additional bits
                lda distance_table_additional_bits, x
                jsr decomp_read_n_bit_value
                //  Subtract additional bits from backref_ptr
                lda backref_ptr_low
                sec
                sbc read_n_bit_value_low
                sta backref_ptr_low
                lda backref_ptr_high
                sbc read_n_bit_value_high
                sta backref_ptr_high

                // Copy backref_length bytes from backref_ptr to output
                ldx backref_length_low
decomp_copy_bytes:
                    // Copy and output byte
                    lda (backref_ptr_low), y
                    jsr decomp_write_byte

                    // Increment backref_ptr
                    inc backref_ptr_low
                    bne !+
                        inc backref_ptr_high

                // Decrement backref_length
                //  Unfortunately, dex doesn't set carry, so we have to detect it ourselves manually
!:              cpx #$00
                bne !+
                    dec backref_length_high
!:              dex

                // Check backref_length for 0. If we're not 0, copy some more bytes
                bne decomp_copy_bytes
                cpy backref_length_high
                bne decomp_copy_bytes
                beq decomp_next // Logically this should be jmp, but beq is equivalent here (due to cpy above) and 1 byte smaller

decomp_literal:
                // Literal
                //  Read byte
                tya
                ldx #$08
!:                  asl
                    sta read_bit_temp
                    jsr decomp_read_bit
                    ora read_bit_temp
                dex
                bne !-

                jsr decomp_write_byte

decomp_next:
        lda decomp_write_instr + 1
        cmp #<uncompressed_intro_end
        bne !+
        lda decomp_write_instr + 2
        cmp #>uncompressed_intro_end
        bne !+
        jmp intro_entry

!:      jmp decomp_packet

        // Ensures n and z flags are set along with returning the bit in a
decomp_read_bit:
        lda bit_index
        and #$07
        bne !+
decomp_read_instr:
            lda packed_intro_start
            sta bit_buffer

            inc decomp_read_instr + 1
            bne !+
                inc decomp_read_instr + 2
!:      lda bit_buffer
        lsr bit_buffer
        inc bit_index
        and #$01
        rts

        // Reads unary value into a and x
decomp_read_unary:
        ldx #$00
!:          jsr decomp_read_bit
        bne !+
            inx
        bne !- // Logically this should be jmp, but bne is equivalent here (due to inx before) and 1 byte smaller
!:      txa
        rts

        // Expects number of bits to read in a
        //  Reads into read_n_bit_value_low/high, and leaves read_n_bit_value_low in a
        //  Does not _expect_ y to be zero since it's also called when decoding the table, but doesn't touch y either
decomp_read_n_bit_value:
        ldx #$00
        stx read_n_bit_value_low
        stx read_n_bit_value_high
        tax
        beq decomp_read_n_bit_value_done
!:          asl read_n_bit_value_low
            rol read_n_bit_value_high
            jsr decomp_read_bit
            ora read_n_bit_value_low
            sta read_n_bit_value_low
        dex
        bne !-
decomp_read_n_bit_value_done:
        rts

decomp_write_byte:
decomp_write_instr:
        sta uncompressed_start_addr

        inc decomp_write_instr + 1
        bne !+
            inc decomp_write_instr + 2
!:      rts

packed_intro_start:
        .import binary "packed-intro.bin"
packed_intro_end:
    }

    .const target_size = $1000
    .const max_size = $2000

    .var total_size = * - start_addr + 2 // + 2 to include the program load address
    .if(total_size > max_size) {
        .error "Total size (" + total_size + " bytes) is more than the max size (" + max_size + " bytes)!"
    }
    .print "Total size: " + total_size + " bytes"
    .if(total_size > target_size) {
        .print "WARNING: Total size greater than target size (" + target_size + " bytes) by " + (total_size - target_size) + " bytes!"
    }
