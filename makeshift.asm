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

    // Place @ character at end of screen (we'll use this as a progress indicator)
    lda #$00
    sta $0400 + 999

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

    .const initial_predictions = 110

    .const match_len_bits = 8
    .const match_offset_bits = 11
    .const literal_bits = 8

    .const contexts = $c000

    .const num_match_len_contexts = 1 << match_len_bits
    .const match_len_contexts = contexts
    .const num_match_offset_contexts = 1 << match_offset_bits
    .const match_offset_contexts = match_len_contexts + num_match_len_contexts
    .const num_literal_contexts = 1 << literal_bits
    .const literal_contexts = match_offset_contexts + num_match_offset_contexts
    .const num_total_model_contexts = num_match_len_contexts + num_match_offset_contexts + num_literal_contexts

    .const contexts_end = contexts + num_total_model_contexts

    .const context = $92
    .const context_low = $92
    .const context_high = $93

    .const decode_bits_contexts = $94
    .const decode_bits_contexts_low = $94
    .const decode_bits_contexts_high = $95

    .const output = $96
    .const output_low = $96
    .const output_high = $97

    .const input = $a3
    .const input_low = $a3
    .const input_high = $a4

    .const bit_buffer = $a5
    .const bit_buffer_pos = $a6

    .const read_bits_count = $a7
    .const read_bits_low = $a8
    .const read_bits_high = $a9

    .const x_low = $aa
    .const x_high = $ab

    .const symbol = $ac
    .const bias = $ad
    .const prediction = $ae

    .const product_low = $af
    .const product_high = $b0

    .const context_bits_low = $f7
    .const context_bits_high = $f8

    .const offset = $f9
    .const offset_low = $f9
    .const offset_high = $fa
    .const last_offset_low = $fb
    .const last_offset_high = $fc
    .const length = $fd

    .pc = * "decompressor and packed intro"
decomp_start:
    .pseudopc reloc_addr {
decomp_entry:
        // Expects y to be 0 on entry
        lda #<packed_intro_start
        sta input_low
        lda #>packed_intro_start
        sta input_high
        lda #<uncompressed_start_addr
        sta output_low
        lda #>uncompressed_start_addr
        sta output_high

        // Load initial state
        jsr decode_input_byte
        sta x_low
        jsr decode_input_byte
        sta x_high

        sty bit_buffer_pos

        // Set up initial model predictions
        lda #<contexts
        sta context_low
        lda #>contexts
        sta context_high
initial_predictions_loop:
            lda #initial_predictions
            sta (context), y
            inc context_low
            bne !+
                inc context_high
!:      lda context_low
        cmp #<contexts_end
        bne initial_predictions_loop
        lda context_high
        cmp #>contexts_end
        bne initial_predictions_loop

packet_loop:
            // Progress indicator
            inc $d800 + 999

            lda #<match_len_contexts
            sta decode_bits_contexts_low
            lda #>match_len_contexts
            sta decode_bits_contexts_high
            lda #match_len_bits
            jsr decode_read_bits
            beq literal

match:
            sta length

            lda #<match_offset_contexts
            sta decode_bits_contexts_low
            lda #>match_offset_contexts
            sta decode_bits_contexts_high
            lda #match_offset_bits
            jsr decode_read_bits

            lda read_bits_low
            sta offset_low
            lda read_bits_high
            sta offset_high

            bne !+
            lda offset_low
            bne !+
                lda last_offset_low
                sta offset_low
                lda last_offset_high
                sta offset_high
!:          lda offset_low
            sta last_offset_low
            lda offset_high
            sta last_offset_high

            sec
            lda output_low
            sbc offset_low
            sta offset_low
            lda output_high
            sbc offset_high
            sta offset_high

match_copy_loop:
                lda (offset), y
                jsr decode_write_byte
                inc offset_low
                bne !+
                    inc offset_high
!:          dec length
            bne match_copy_loop

            jmp next_packet

literal:
            lda #<literal_contexts
            sta decode_bits_contexts_low
            lda #>literal_contexts
            sta decode_bits_contexts_high
            lda #literal_bits
            jsr decode_read_bits
            jsr decode_write_byte

next_packet:
            lda output_low
            cmp #<uncompressed_intro_end
            bne !+
            lda output_high
            cmp #>uncompressed_intro_end
            bne !+
                jmp intro_entry
!:          jmp packet_loop

        // outputs read low bits in a, expects y = 0 and contexts in decode_bits_contexts, clobbers a
decode_read_bits:
        sta read_bits_count
        sty read_bits_low
        sty read_bits_high
        sty context_bits_low
        sty context_bits_high
        inc context_bits_low

read_bits_loop:
            clc
            lda decode_bits_contexts_low
            adc context_bits_low
            sta context_low
            lda decode_bits_contexts_high
            adc context_bits_high
            sta context_high
            jsr decode_bit
            php
            rol read_bits_low
            rol read_bits_high
            plp
            rol context_bits_low
            rol context_bits_high
        dec read_bits_count
        bne read_bits_loop

        lda read_bits_low

        rts

        // outputs bit in carry, expects y = 0, clobbers a and x
decode_bit:
        sty symbol

        lda (context), y
        sta prediction

        cmp x_low
        bcc bit_0
        beq bit_0

bit_1:
        sec
        tya
        sbc prediction
        lsr
        lsr
        lsr
        lsr
        bne !+
            lda #$01
!:      clc
        adc prediction
        bne !+
            lda #$ff
!:      sta (context), y

        sty bias
        inc symbol
        jmp update_state

bit_0:
        lsr
        lsr
        lsr
        lsr
        bne !+
            lda #$01
!:      sta bias // use bias as temp here; we'll overwrite it shortly
        lda prediction
        sec
        sbc bias
        bne !+
            lda #$01
!:      sta (context), y

        lda prediction
        sta bias
        sec
        tya
        sbc prediction
        sta prediction

update_state:
        // product = x_high * prediction (clobbers x_high)
        tya
        ldx #$08
        clc
m0:     bcc m1
        clc
        adc prediction
m1:     ror
        ror x_high
        dex
        bpl m0
        sta product_high
        lda x_high

        // add back state low bits
        clc
        adc x_low
        sta product_low
        tya
        adc product_high
        sta product_high

        // subtract bias
        sec
        lda product_low
        sbc bias
        sta x_low
        lda product_high
        sbc #$00
        sta x_high

renormalize:
        bmi renormalize_done
            jsr decode_input_bit
            rol x_low
            rol x_high
        bpl renormalize
renormalize_done:

        ror symbol

        rts

        // outputs bit in carry, expects y = 0, clobbers a
decode_input_bit:
        lda bit_buffer_pos
        bne input_bit
            jsr decode_input_byte
            sta bit_buffer
            lda #$08
            sta bit_buffer_pos
input_bit:
        dec bit_buffer_pos
        ror bit_buffer
        rts

        // outputs byte in a, expects y = 0
decode_input_byte:
        lda (input), y
        inc input_low
        bne !+
            inc input_high
!:      rts

        // writes byte in a, expects y = 0
decode_write_byte:
        sta (output), y

        inc output_low
        bne !+
            inc output_high
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
