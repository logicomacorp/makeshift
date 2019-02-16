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

    .const reloc_addr = $3000

    .pc = * "stub entry"
entry:
    // Disable interrupts
    //  Technically we should be enabling them again after the decompression routine,
    //  but since we know we'll jump back into code that'll disable them anyways, it
    //  should be fine.
    sei

    // Place @ character at end of screen (we'll use this as a progress indicator)
    ldy #$00
    sty $0400 + 999

    // Copy 8kb block starting at the packed data to $8000-$9fff in 32 256-byte chunks
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

    // All of these must be at least 8 (code assumes this for alignment reasons)
    .const match_len_bits = 8
    .const match_offset_bits = 12
    .const literal_bits = 8

    .const contexts = $5000

    .const num_match_len_contexts = 1 << match_len_bits
    .const match_len_contexts = contexts
    .const num_match_offset_contexts = 1 << match_offset_bits
    .const match_offset_contexts = match_len_contexts + num_match_len_contexts
    .const num_literal_contexts = 1 << literal_bits
    .const literal_contexts = match_offset_contexts + num_match_offset_contexts
    .const num_total_model_contexts = num_match_len_contexts + num_match_offset_contexts + num_literal_contexts

    .const contexts_end = contexts + num_total_model_contexts

    .const temp = $02

    .const context = $92
    .const context_low = $92
    .const context_high = $93

    .const decode_bits_contexts = $94

    .const output = $95
    .const output_low = $95
    .const output_high = $96

    .const bit_buffer = $a3
    .const bit_buffer_pos = $a4

    .const read_bits_count = $a5
    .const read_bits = $a6
    .const read_bits_low = $a6
    .const read_bits_high = $a7

    .const x_low = $a8
    .const x_high = $a9

    .const symbol = $aa
    .const bias = $ab
    .const prediction = $ac

    .const context_bits_high = $f7

    .const last_offset_low = $f8
    .const last_offset_high = $f9
    .const length = $fa

    .pc = * "decompressor and packed intro"
decomp_start:
    .pseudopc reloc_addr {
decomp_entry:
        // Expects y to be 0 on entry

        // Load final coder state
        .var final_coder_state = LoadBinary("final-coder-state.bin")
        lda #(final_coder_state.get(0))
        sta x_low
        lda #(final_coder_state.get(1))
        sta x_high

        sty bit_buffer_pos

        // Set initial predictions in 256-byte chunks
        lda #initial_predictions
        ldx #(num_total_model_contexts / 256)
initial_predictions_store_instr:
!:              sta contexts, y
            iny
            bne !-

            inc initial_predictions_store_instr + 2
        dex
        bne !-

packet_loop:
            // Progress indicator
            inc $d800 + 999

            // Read length
            ldx #>match_len_contexts
            lda #match_len_bits
            jsr decode_read_bits
            beq literal

match:
            sta length

            // Read offset
            ldx #>match_offset_contexts
            lda #match_offset_bits
            jsr decode_read_bits

            // If offset == 0, offset = last offset
            bne !+
            lda read_bits_high
            bne !+
                lda last_offset_low
                sta read_bits_low
                lda last_offset_high
                sta read_bits_high
            // offset = last offset
!:          lda read_bits_low
            sta last_offset_low
            lda read_bits_high
            sta last_offset_high

            // offset = output pos - offset
            sec
            lda decode_write_byte_store_instr + 1
            sbc read_bits_low
            sta read_bits_low
            lda decode_write_byte_store_instr + 2
            sbc read_bits_high
            sta read_bits_high

            // Copy match bytes
match_copy_loop:
                lda (read_bits), y
                jsr decode_write_byte
                inc read_bits_low
                bne !+
                    inc read_bits_high
!:          dec length
            bne match_copy_loop

            beq next_packet

literal:
            // Read and output literal
            ldx #>literal_contexts
            lda #literal_bits
            jsr decode_read_bits
            jsr decode_write_byte

next_packet:
            lda decode_write_byte_store_instr + 1
            cmp #<uncompressed_intro_end
            bne !+
            lda decode_write_byte_store_instr + 2
            cmp #>uncompressed_intro_end
            bne !+
                jmp intro_entry
!:          jmp packet_loop

        // outputs read low bits in a, expects y = 0 and contexts high byte in x, clobbers a and x
decode_read_bits:
        sta read_bits_count
        stx decode_bits_contexts
        sty read_bits_low
        //sty read_bits_high // Since we always read at least 8 bits, we don't need to clear this actually
        sty context_low
        sty context_bits_high
        inc context_low

read_bits_loop:
            clc
            lda decode_bits_contexts
            adc context_bits_high
            sta context_high

            // Decode bit
            sty symbol

            lda (context), y
            sta prediction

            cmp x_low
            bcc bit_0
            beq bit_0

bit_1:
            // Set bias
            sty bias

            // Update model
            sec
            tya
            sbc prediction
            jsr calc_prediction_adjustment
            clc
            adc prediction
            bcc !+
                lda #$ff
!:          sta (context), y

            // Symbol = 1
            inc symbol
            bne update_state

bit_0:
            // Set bias
            sta bias

            // Update model
            jsr calc_prediction_adjustment
            sta temp
            lda prediction
            sec
            sbc temp
            bne !+
                lda #$01
!:          sta (context), y

            // Flip prediction
            sec
            tya
            sbc prediction
            sta prediction

update_state:
            // product = x_high * prediction (clobbers x_high)
            tya
            ldx #$08
            clc
m0:         bcc m1
            clc
            adc prediction
m1:         ror
            ror x_high
            dex
            bpl m0
            sta temp
            lda x_high

            // add back state low bits
            clc
            adc x_low
            sta x_low
            tya
            adc temp
            pha

            // subtract bias
            sec
            lda x_low
            sbc bias
            sta x_low
            pla
            sbc #$00
            sta x_high

renormalize:
            bmi renormalize_done
renormalize_loop:
                // Input bit
                lda bit_buffer_pos
                bne read_bit
decode_input_byte_load_instr:
                    lda packed_intro_start
                    inc decode_input_byte_load_instr + 1
                    bne !+
                        inc decode_input_byte_load_instr + 2
!:                  sta bit_buffer
                    lda #$08
                    sta bit_buffer_pos
read_bit:
                dec bit_buffer_pos
                ror bit_buffer

                // Shift into state
                rol x_low
                rol x_high
            bpl renormalize_loop
renormalize_done:

            // Shift bit into read_bits and context
            ror symbol
            php
            rol read_bits_low
            rol read_bits_high
            plp
            rol context_low
            rol context_bits_high
        dec read_bits_count
        beq !+
            jmp read_bits_loop

!:      lda read_bits_low

        rts

        // Expects prediction in a
calc_prediction_adjustment:
        lsr
        lsr
        lsr
        lsr
        bne !+
            lda #$01
!:      rts

        // writes byte in a
decode_write_byte:
decode_write_byte_store_instr:
        sta uncompressed_start_addr

        inc decode_write_byte_store_instr + 1
        bne !+
            inc decode_write_byte_store_instr + 2
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
