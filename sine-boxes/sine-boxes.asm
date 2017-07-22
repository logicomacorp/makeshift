#import "../common.inc"
#import "sine-boxes.inc"
#import "../layers/layers.inc"

    .const dynamic_tab = $9200
    .const bg_update_speedcode = dynamic_tab + $200

    .const vic_bank_offset = $8000

    .const screen_mem_offset = vic_bank_offset + $3400
    .const sprite_pointer_offset = screen_mem_offset + $03f8
    .const charset_offset = vic_bank_offset + $3800
    .const left_sprite = vic_bank_offset + $3380
    .const right_sprite = left_sprite + $40

    .const zp_base = $60

    .const sine_boxes_plasma_index = zp_base
    .const sine_boxes_bg_index = zp_base + 1
    .const sine_boxes_x_move_index = zp_base + 2
    .const sine_boxes_y_move_index = zp_base + 3
    .const sine_boxes_x_move_add = zp_base + 4
    .const sine_boxes_y_move_add = zp_base + 5

    .const temp_pos_x = zp_base + 6
    .const temp_pos_y = zp_base + 7
    .const temp_char_x = zp_base + 8
    .const temp_char_y = zp_base + 9
    .const temp_acc = zp_base + 10
    .const temp_tiles_row_low = zp_base + 11
    .const temp_tiles_row_high = zp_base + 12

    .const temp_tile_x = zp_base + 6
    .const temp_tile_y = zp_base + 7

    .const temp_imm = zp_base + 8

    .const temp_add_1 = zp_base + 6
    .const temp_add_2 = zp_base + 7

    .const temp_x_move_index = zp_base + 8
    .const temp_y_move_index = zp_base + 9

    .pc = sine_boxes_code_base "interface"
    jmp init_bg
    jmp init_int

    .pc = * "init bg"
init_bg:
    // Unpack bg chars
    lda #<charset_offset
    sta block_write_addr_low
    lda #>charset_offset
    sta block_write_addr_high

    ldx #$00
unpack_bg_chars_pos_y:
        txa
        pha

        stx temp_pos_y

        ldx #$00
unpack_bg_chars_pos_x:
            txa
            pha

            stx temp_pos_x

            ldx #$00
unpack_bg_chars_char_y:
                txa
                pha

                stx temp_char_y

                ldx #$00
unpack_bg_chars_char_x:
                    txa
                    pha

                    stx temp_char_x

                    lda temp_char_y
                    clc
                    adc temp_pos_y
                    and #$0f
                    asl
                    tax
                    lda packed_charset, x
                    sta temp_tiles_row_high
                    inx
                    lda packed_charset, x
                    sta temp_tiles_row_low

                    lda temp_char_x
                    clc
                    adc temp_pos_x
                    and #$0f
                    tax
                    beq unpack_bg_chars_shift_done
!:                      lsr temp_tiles_row_high
                        ror temp_tiles_row_low
                    dex
                    bne !-

unpack_bg_chars_shift_done:
                    lda temp_tiles_row_low
                    lsr
                    rol temp_acc

                    pla
                    tax
                inx
                cpx #$08
                bne unpack_bg_chars_char_x

                lda temp_acc
                jsr block_write_byte

                pla
                tax
            inx
            cpx #$08
            bne unpack_bg_chars_char_y

            pla
            tax
        inx
        cpx #$10
        bne unpack_bg_chars_pos_x

        pla
        tax
    inx
    cpx #$10
    bne unpack_bg_chars_pos_y

    // Generate bg update speedcode
    /*.for(var tile_y = 0; tile_y < 25; tile_y++) {
        .for(var tile_x = 0; tile_x < 40; tile_x++) {
            lda #(tile_x * 4 + ((tile_y * 8) << 4))
            adc dynamic_tab + ((tile_x + tile_y / 2) & $ff), x
            sta screen_mem_offset + tile_y * 40 + tile_x
        }
    }*/
    lda #<bg_update_speedcode
    sta block_write_addr_low
    lda #>bg_update_speedcode
    sta block_write_addr_high
    ldx #$00
gen_bg_update_speedcode_y:
        txa
        pha

        stx temp_tile_y

        ldx #$00
gen_bg_update_speedcode_x:
            txa
            pha

            stx temp_tile_x

            // Output lda
            lda #LDA_IMM
            jsr block_write_byte

            // Output lda imm
            lda temp_tile_y

            lsr
            ror

            sta temp_imm

            lda temp_tile_x
            asl
            asl
            asl
            clc
            adc temp_imm

            jsr block_write_byte

            // Output adc
            lda #ADC_ABSX
            jsr block_write_byte

            // Output adc addr low
            lda temp_tile_y
            lsr
            clc
            adc temp_tile_x
            jsr block_write_byte

            // Output adc addr high
            lda #>dynamic_tab
            jsr block_write_byte

            // Output sta
            lda #STA_ABS
            jsr block_write_byte

            // Output sta addr low
gen_update_bg_speedcode_sta_addr_low_instr:
            lda #<screen_mem_offset
            jsr block_write_byte

            // Output sta addr high
gen_update_bg_speedcode_sta_addr_high_instr:
            lda #>screen_mem_offset
            jsr block_write_byte

            // Increment sta addr
            inc gen_update_bg_speedcode_sta_addr_low_instr + 1
            bne !+
                inc gen_update_bg_speedcode_sta_addr_high_instr + 1

!:          pla
            tax
        inx
        cpx #40
        bne gen_bg_update_speedcode_x

        pla
        tax
    inx
    cpx #25
    bne gen_bg_update_speedcode_y
    // Write rts at the end
    lda #RTS
    jsr block_write_byte

    // Copy and shift first low tab over all tabs (including itself, since we're going to be doing a small fixup)
    //  We want our tables to be repeated since we're reading a bit past their bases,
    //  and want to make sure they "wrap" like we expect
    ldx #$00
tab_copy_shift_outer:
        txa
        pha

        and #$fe
        sta tab_copy_shift_shift_instr + 1

        ldx #$00
tab_copy_shift_inner:
            lda sine_boxes_plasma_tab_4_low, x

            // Eliminate zero values
            //  Shifting in the case of this table actually ends up with a range of [-2, 1] before bias,
            //  which yields [0, 3] after bias rather than [1, 3] that we want
            bne !+
                lda #$01

tab_copy_shift_shift_instr:
!:          ldy #$00
            beq tab_copy_shift_shift_done
!:              asl
            dey
            bne !-

tab_copy_shift_shift_done:
tab_copy_shift_store_instr:
            sta sine_boxes_plasma_tab_4_low, x
        inx
        bne tab_copy_shift_inner

        inc tab_copy_shift_store_instr + 2

        pla
        tax
    inx
    cpx #$08
    bne tab_copy_shift_outer

    // Generate dynamic tab
    ldx #$00
!:      txa

        asl
        tay
        lda sine_boxes_dynamic_base_tab, y
        sta temp_add_1

        txa
        asl
        asl
        tay
        lda sine_boxes_dynamic_base_tab, y
        asl
        asl
        asl
        asl

        clc
        adc temp_add_1

        sta dynamic_tab, x
        sta dynamic_tab + $100, x
    inx
    bne !-

    // Init variables
    lda #$00
    sta sine_boxes_plasma_index
    sta sine_boxes_bg_index
    sta sine_boxes_x_move_index
    sta sine_boxes_y_move_index
    lda #$f0
    sta sine_boxes_x_move_add
    sta sine_boxes_y_move_add

    // Set up sprite data pointers
    ldx #<((left_sprite - vic_bank_offset) / $40)
    .for(var i = 0; i < 4; i++) {
        stx sprite_pointer_offset + i * 2
    }
    inx
    .for(var i = 0; i < 4; i++) {
        stx sprite_pointer_offset + 1 + i * 2
    }

    // Set up color mem
    //  TODO: uhhh when do we do this? :D
    /*lda #$09
    ldx #$00
!:      sta $d800, x
        sta $d900, x
        sta $da00, x
        sta $db00, x
    inx
    bne !-*/

    rts

    .pc = * "init int"
init_int:
    // Set up frame interrupt
    lda #<frame
    sta $fffe
    lda #>frame
    sta $ffff
    lda #$ff
    sta $d012

    // Reset cycle counter
    lda #$00
    sta cycle_counter

    rts

    .pc = * "frame"
frame:
    pha
    txa
    pha
    tya
    pha
    lda $01
    pha

    // Bank in io regs
    lda #$35
    sta $01

    // Reset graphics mode etc
    lda #$1b
    sta $d011

    lda #$c8
    sta $d016

    // Set VIC bank
    lda #$c5
    sta $dd00

    // Set charset pointer
    lda #$de
    sta $d018

    // Enable sprites
    lda #$ff
    sta $d015

    // Set double height for all sprites
    sta $d017

    // Set multicolor mode for all sprites
    sta $d01c

    // Set bg color
    ldx cycle_counter
    lda bg_end_colors, x
    sta $d021
    beq !+
        lda cycle_frame_counter
        lsr
        tax
        lda color_ramp, x
        cmp #$01
        beq !+
            sta $d021

    // Set sprite colors
!:  lda #$00
    sta $d025
    lda cycle_frame_counter
    lsr
    tax
    lda color_ramp, x
    sta $d026
    lda cycle_frame_counter
    lsr
    tax
    lda color_ramp, x
    cmp #$01
    bne !+
        lda #$04
!:  sta $d027
    sta $d028
    lda cycle_frame_counter
    lsr
    tax
    lda color_ramp, x
    cmp #$01
    bne !+
        lda #$0a
!:  sta $d029
    sta $d02a
    lda cycle_frame_counter
    lsr
    tax
    lda color_ramp, x
    cmp #$01
    bne !+
        lda #$05
!:  sta $d02b
    sta $d02c
    lda cycle_frame_counter
    lsr
    tax
    lda color_ramp, x
    cmp #$01
    bne !+
        lda #$0e
!:  sta $d02d
    sta $d02e

    // Update music
    //inc $d020
    jsr music + 3
    //dec $d020

    // If we're on our first frame, clear first row in color mem and dispatch layers loading on BG thread
    lda cycle_counter
    bne update
    lda cycle_frame_counter
    bne update
        lda #$00
        tax
!:          sta $d800, x
        inx
        cpx #40
        bne !-

        inc bg_thread_dispatch

update:
    inc sine_boxes_plasma_index
    inc sine_boxes_bg_index

    inc sine_boxes_x_move_index
    dec sine_boxes_y_move_index

    lda sine_boxes_x_move_index
    sta temp_x_move_index
    lda sine_boxes_y_move_index
    sta temp_y_move_index

    // Reset sprite x high bits
    lda #$00
    sta $d010

    // Update sprite positions
    tay
!:      ldx temp_x_move_index
        lda sine_boxes_sprite_offset_x_tab_low, x
        sec
        sbc #$0c
        sta $d000, y
        lda sine_boxes_sprite_offset_x_tab_high, x
        sbc #$00
        lsr
        ror $d010

        lda sine_boxes_sprite_offset_x_tab_low, x
        clc
        adc #$0c
        sta $d002, y
        lda sine_boxes_sprite_offset_x_tab_high, x
        adc #$00
        lsr
        ror $d010

        ldx temp_y_move_index
        lda sine_boxes_sprite_offset_y_tab_low, x
        sta $d001, y
        sta $d003, y

        lda temp_x_move_index
        clc
        adc sine_boxes_x_move_add
        sta temp_x_move_index

        lda temp_y_move_index
        clc
        adc sine_boxes_y_move_add
        sta temp_y_move_index
    iny
    iny
    iny
    iny
    cpy #$10
    bne !-

    // Update plasma
    //dec $d020

    // Left side
    ldx #$00

    lda #$55
    .for(var i = 0; i < 3; i++) {
        sta left_sprite, x
        inx
    }

!:      lda sine_boxes_plasma_index
        clc
        adc sine_boxes_plasma_y_tab, x
        tay
        clc
        adc sine_boxes_plasma_y_tab, y

        asl
        asl
        tay

        lda #(1 << 6)
        ora sine_boxes_plasma_tab_2, y
        ora sine_boxes_plasma_tab_3 + 1 * 4, y
        ora sine_boxes_plasma_tab_4 + 2 * 4, y
        sta left_sprite, x
        inx
        lda sine_boxes_plasma_tab_1 + 3 * 4, y
        ora sine_boxes_plasma_tab_2 + 4 * 4, y
        ora sine_boxes_plasma_tab_3 + 5 * 4, y
        ora sine_boxes_plasma_tab_4 + 6 * 4, y
        sta left_sprite, x
        inx
        lda sine_boxes_plasma_tab_1 + 7 * 4, y
        ora sine_boxes_plasma_tab_2 + 8 * 4, y
        ora sine_boxes_plasma_tab_3 + 9 * 4, y
        ora sine_boxes_plasma_tab_4 + 10 * 4, y
        sta left_sprite, x
        inx

    cpx #$3c
    bne !-

    lda #$55
    .for(var i = 0; i < 3; i++) {
        sta left_sprite, x
        inx
    }

    // Right side
    ldx #$00

    lda #$55
    .for(var i = 0; i < 3; i++) {
        sta right_sprite, x
        inx
    }

!:      lda sine_boxes_plasma_index
        clc
        adc sine_boxes_plasma_y_tab, x
        tay
        clc
        adc sine_boxes_plasma_y_tab, y

        clc
        adc #$0c

        asl
        asl
        tay

        lda sine_boxes_plasma_tab_1, y
        ora sine_boxes_plasma_tab_2 + 1 * 4, y
        ora sine_boxes_plasma_tab_3 + 2 * 4, y
        ora sine_boxes_plasma_tab_4 + 3 * 4, y
        sta right_sprite, x
        inx
        lda sine_boxes_plasma_tab_1 + 4 * 4, y
        ora sine_boxes_plasma_tab_2 + 5 * 4, y
        ora sine_boxes_plasma_tab_3 + 6 * 4, y
        ora sine_boxes_plasma_tab_4 + 7 * 4, y
        sta right_sprite, x
        inx
        lda sine_boxes_plasma_tab_1 + 8 * 4, y
        ora sine_boxes_plasma_tab_2 + 9 * 4, y
        ora sine_boxes_plasma_tab_3 + 10 * 4, y
        ora #1
        sta right_sprite, x
        inx

    cpx #$3c
    bne !-

    lda #$55
    .for(var i = 0; i < 3; i++) {
        sta right_sprite, x
        inx
    }

    //inc $d020

    // Update bg
    //inc $d020
    //inc $d020

    ldx sine_boxes_bg_index
    clc
    jsr bg_update_speedcode

    //dec $d020
    //dec $d020

    // Update cycle
    inc cycle_frame_counter
    lda cycle_frame_counter
    cmp #cycle_frames
    bne cycle_update_done
        // Bump cycle counter
        inc cycle_counter

        // Reset counter
        lda #$00
        sta cycle_frame_counter

        // If we've gone 8 cycles, time to fire up the next effect
        lda cycle_counter
        cmp #$08
        bne !+
            jsr layers_init_int

!:      lda sine_boxes_x_move_add
        clc
        adc #$3b
        sta sine_boxes_x_move_add

        lda sine_boxes_y_move_add
        clc
        adc #$c7
        sta sine_boxes_y_move_add

        // TODO: More stuff based on cycles or something. :D

cycle_update_done:
    asl $d019
    pla
    sta $01
    pla
    tay
    pla
    tax
    pla
    rti

    .file [name="sine-boxes-data.prg", segments="Data"]
    .segmentdef Data [start=sine_boxes_data_base]
    .segment Data "data"

    .pc = * "packed charset"
packed_charset:
    .import binary "tiles.bin"

    .pc = * "color ramp"
color_ramp:
    .byte $00, $00, $06, $04, $03, $01
    .for(var i = 0; i < cycle_frames / 2 - 12; i++) {
        .byte $01
    }
    .byte $01, $03, $04, $06, $00, $00

bg_end_colors:
    .byte $00, $00, $06, $04, $0b, $06, $04, $00
