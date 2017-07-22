#import "common.inc"
#import "basic.inc"

#import "fld-text/fld-text.inc"
#import "zoom-plasma/zoom-plasma.inc"
#import "layers/layers.inc"
#import "sine-boxes/sine-boxes.inc"

    // It seems the BASIC interpreter is pretty lax with the next line addresses, so long as
    //  they're within the BASIC program space, so it should pack better if we just use the
    //  same address over and over (and same thing for the line numbers as well) :D
    .pc = basic_reentry "BASIC reentry point"

    // for a = 0 to 2 * pi step 1 / 128 * pi
    //  Note that this loop range is inclusive, so we may get a couple extra values at the end, but
    //  the write pointers should wrap and overwrite the first couple values, which should be the
    //  same anyways.
    StartBasicLine(basic_reentry, 0, basic_token_for)
    .text "A"
    .byte basic_token_equals
    .text "0"
    .byte basic_token_to
    .text "2"
    .byte basic_token_multiplication
    .byte basic_token_pi
    .byte basic_token_step
    .text "1"
    .byte basic_token_division
    .text "128"
    .byte basic_token_multiplication
    .byte basic_token_pi
    EndBasicLine()
    // s = sin(a)
    StartBasicLineWithoutToken(basic_reentry, 0)
    .text "S%"
    .byte basic_token_equals
    .byte basic_token_sin
    .text "(A)"
    .byte basic_token_multiplication
    .text "32767"
    .byte basic_token_addition
    .text "0.5"
    EndBasicLine()
    // sys store_s
    StartBasicLine(basic_reentry, 0, basic_token_sys)
    .text store_s
    EndBasicLine()
    // next i
    StartBasicLine(basic_reentry, 0, basic_token_next)
    .text "A"
    EndBasicLine()

    // sys intro_init
    StartBasicLine(basic_reentry, 0, basic_token_sys)
    .text intro_init
    EndBasicLine()
    EndBasicProgram()

    .pc = $853 "vartab"
vartab:
    EmptyRealVar("A")
vartab_s:
    EmptyIntegerVar("S")

    .pc = intro_entry "intro entry"
    // This code is executed directly after unpacking. Assumes interrupts are disabled and BASIC and kernal ROMs banked in.
    //  At this point we've still got some more work to do in BASIC land, so we'll just fix up the VARTAB and dive back in.

    // Fix up VARTAB after decompression extended the BASIC program
    lda #<vartab
    sta $2d
    lda #>vartab
    sta $2e

    // Return to BASIC
    rts

    .pc = * "store tabvars"
store_s:
    // Pull data out of s in the VARTAB and stuff it into the first sine table
    lda vartab_s + 3
!:  sta sine_tables
    inc !- + 1
    lda vartab_s + 2
!:  sta sine_tables + $100
    inc !- + 1

    // Wait for scanline 0 before making any visual changes
!:      lda $d012
    bne !-
        lda $d011
        and #$80
    bne !-

    // Clear some screen chars
!:  ldx #$00
    lda #160
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$00
    sta $d800, x
    sta $d900, x
    sta $da00, x
    sta $db00, x
    inc !- + 1

    txa
    cmp #(256 - 10)
    bcc !+
        sec
        sbc #(256 - 10)
        tax
        lda store_s_coltab, x
        sta $d020

!:  rts

    .pc = * "intro init"
intro_init:
    // Finally done with unpacking+BASIC init; let's get this show on the road :)
    //  Note that some of this setup was done in the intro_entry routine already, but since
    //  we just came back from BASIC again, let's make sure these are set up as expected so
    //  we're guaranteed a clean state.
    sei

    // Ack CIA interrupts
    lda $dc0d
    lda $dd0d

    // Ack VIC interrupts
    asl $d019

    // Bank out BASIC and kernal ROMs
    lda #$35
    sta $01

    //jsr mask_nmi

    // Turn off CIA interrupts
    lda #$7f
    sta $dc0d
    sta $dd0d

    // Enable raster interrupts
    lda #$01
    sta $d01a

    // Reset screen settings
    lda #$1b
    sta $d011

    // Reset background colors
    lda #$00
    sta $d020
    sta $d021

    // Init music
    lda #$00
    tax
    tay
    jsr music

    // Copy first sine table into the other ones before scaling
    ldx #$00
    ldy #$00
tab_copy:
tab_copy_load_instr:
!:          lda sine_tables, x
tab_copy_store_instr:
            sta sine_tables + $200, x
        inx
        bne !-

        inc tab_copy_load_instr + 2
        inc tab_copy_store_instr + 2
    iny
    cpy #(19 * 2)
    bne tab_copy

    // Shift right table values
    ldy #$00
tab_right_shift_loop:
        tya
        pha

        lda tab_right_shift_tab, y
        beq tab_right_shift_shift_done
            tay
tab_right_shift_shift_loop:
                tya
                pha

                ldx #$00
tab_right_shift_tab_high_instr_1:
!:                  lda sine_tables + $100, x
                    tay
                    asl // Place sign into carry
tab_right_shift_tab_high_instr_2:
                    ror sine_tables + $100, x

tab_right_shift_tab_low_instr:
                    ror sine_tables, x
                inx
                bne !-

                pla
                tay
            dey
            bne tab_right_shift_shift_loop

tab_right_shift_shift_done:
        inc tab_right_shift_tab_high_instr_1 + 2
        inc tab_right_shift_tab_high_instr_1 + 2
        inc tab_right_shift_tab_high_instr_2 + 2
        inc tab_right_shift_tab_high_instr_2 + 2
        inc tab_right_shift_tab_low_instr + 2
        inc tab_right_shift_tab_low_instr + 2

        pla
        tay
    iny
    cpy #$13
    bne tab_right_shift_loop

    // Invert x_offset table
    ldx #$00
!:      lda #$00
        sec
        sbc zoom_plasma_x_offset_low_tab, x
        sta zoom_plasma_x_offset_low_tab, x
        lda #$00
        sbc zoom_plasma_x_offset_high_tab, x
        sta zoom_plasma_x_offset_high_tab, x
    dex
    bne !-

    // Invert y_offset table
    ldx #$00
!:      lda #$00
        sec
        sbc zoom_plasma_y_offset_low_tab, x
        sta zoom_plasma_y_offset_low_tab, x
        lda #$00
        sbc zoom_plasma_y_offset_high_tab, x
        sta zoom_plasma_y_offset_high_tab, x
    dex
    bne !-

    // Bias table values
    ldy #$00
tab_bias_loop:
        ldx #$00
tab_bias_tab_low_instr_1:
!:          lda sine_tables, x
            clc
            adc tab_bias_low_tab, y
tab_bias_tab_low_instr_2:
            sta sine_tables, x
tab_bias_tab_high_instr_1:
            lda sine_tables + $100, x
            adc tab_bias_high_tab, y
tab_bias_tab_high_instr_2:
            sta sine_tables + $100, x
        inx
        bne !-

        inc tab_bias_tab_low_instr_1 + 2
        inc tab_bias_tab_low_instr_1 + 2
        inc tab_bias_tab_low_instr_2 + 2
        inc tab_bias_tab_low_instr_2 + 2
        inc tab_bias_tab_high_instr_1 + 2
        inc tab_bias_tab_high_instr_1 + 2
        inc tab_bias_tab_high_instr_2 + 2
        inc tab_bias_tab_high_instr_2 + 2
    iny
    cpy #$13
    bne tab_bias_loop

    // Effect init
    lda #$01
    jsr fld_text_init_bg
    jsr fld_text_init_int
    //jsr zoom_plasma_init_bg
    //jsr zoom_plasma_init_int
    //jsr layers_init_bg
    //jsr layers_init_int
    //jsr sine_boxes_init_bg
    //jsr sine_boxes_init_int

    cli

    // BG thread

    // Preload zoom plasma+sine boxes while showing fld text
    jsr zoom_plasma_init_bg
    jsr sine_boxes_init_bg

bg_thread_loop:
    lda #$00
    sta bg_thread_dispatch

!:      lda bg_thread_dispatch
    beq !-

    jsr layers_init_bg
    // Let's also go ahead and init our text stuff for the end - shouldn't tread on anything important at least :D
    lda #$00
    jsr fld_text_init_bg
    jmp bg_thread_loop

    /*.pc = * "mask nmi"
mask_nmi:
    // Stop timer A
    lda #$00
    sta $dd0e

    // Set timer A to 0 after starting
    sta $dd04
    sta $dd05

    // Set timer A as NMI source
    lda #$81
    sta $dd0d

    // Set NMI vector
    lda #<masked_nmi
    sta $fffa
    lda #>masked_nmi
    sta $fffb

    // Start timer A (NMI triggers immediately)
    lda #$01
    sta $dd0e

    rts

masked_nmi:
    rti*/

    .pc = * "block manipulation implementation"
    // Copies a bytes from block_read_addr to block_write_addr, and updates block_read_addr and block_write_addr. Assumes a is > 0, clobbers x & y
block_copy_bytes_impl:
    tax
    ldy #$00
!:      lda (block_read_addr), y
        sta (block_write_addr), y
        iny
    dex
    bne !-
    tya
    jsr block_inc_read_addr_impl
    jmp block_inc_write_addr_impl

    // Writes a to block_write_addr and increments block_write_addr. Clobbers y
block_write_byte_impl:
    ldy #$00
    sta (block_write_addr), y
    lda #$01
    jmp block_inc_write_addr_impl

    // Increments block_read_addr by the value in a, preserving a
block_inc_read_addr_impl:
    pha
    clc
    adc block_read_addr_low
    sta block_read_addr_low
    bcc !+
        inc block_read_addr_high
!:  pla
    rts

    // Increments block_write_addr by the value in a, preserving a
block_inc_write_addr_impl:
    pha
    clc
    adc block_write_addr_low
    sta block_write_addr_low
    bcc !+
        inc block_write_addr_high
!:  pla
    rts

    .pc = block_manipulation_interface "block manipulation interface"
    jmp block_copy_bytes_impl
    jmp block_write_byte_impl
    jmp block_inc_read_addr_impl
    jmp block_inc_write_addr_impl

    .pc = music "music"
    .import c64 "music.prg"

    .pc = fld_text_code_base "fld text code"
    .import c64 "fld-text/fld-text.prg"

    .pc = fld_text_data_base "fld text data"
    .import c64 "fld-text/fld-text-data.prg"

    .pc = zoom_plasma_code_base "zoom plasma code"
    .import c64 "zoom-plasma/zoom-plasma.prg"

    .pc = zoom_plasma_data_base "zoom plasma data"
    .import c64 "zoom-plasma/zoom-plasma-data.prg"

    .pc = layers_code_base "layers code"
    .import c64 "layers/layers.prg"

    .pc = layers_data_base "layers data"
    .import c64 "layers/layers-data.prg"

    .pc = sine_boxes_code_base "sine boxes code"
    .import c64 "sine-boxes/sine-boxes.prg"

    .pc = sine_boxes_data_base "sine boxes data"
    .import c64 "sine-boxes/sine-boxes-data.prg"

    .pc = * "table right shift table"
    .const zoom_plasma_x_offset_shift = 2 // Can't overlap
    .const zoom_plasma_x_interval_shift = 6 // Can't overlap
    .const zoom_plasma_x_move_shift = 0
    .const zoom_plasma_x_flow_shift = 9 // Can't overlap
    .const zoom_plasma_y_offset_shift = 3 // Can't overlap
    .const zoom_plasma_y_interval_shift = 9 // Can't overlap
    .const zoom_plasma_y_move_shift = 1
    .const zoom_plasma_y_flow_shift = 9
    .const layers_sin_tab_1_shift = 8
    .const layers_sin_tab_2_shift = 12
    .const sine_boxes_plasma_tab_shift = 14 // Can't overlap
    //.const sine_boxes_plasma_y_tab_shift = 8
    .const sine_boxes_dynamic_base_tab_shift = 10
    .const sine_boxes_sprite_offset_x_tab_shift = 8 // Can't overlap
    .const sine_boxes_sprite_offset_y_tab_shift = 9 // Can't overlap
    .const fld_text_fld_y_tab_shift = 10
tab_right_shift_tab:
    .byte zoom_plasma_x_offset_shift
    .byte zoom_plasma_x_interval_shift
    .byte zoom_plasma_x_move_shift
    .byte zoom_plasma_x_flow_shift
    .byte zoom_plasma_y_offset_shift
    .byte zoom_plasma_y_interval_shift
    .byte zoom_plasma_y_move_shift
    .byte zoom_plasma_y_flow_shift
    .byte layers_sin_tab_1_shift
    .byte layers_sin_tab_2_shift
    .byte sine_boxes_plasma_tab_shift
    .byte sine_boxes_plasma_tab_shift
    .byte sine_boxes_plasma_tab_shift
    .byte sine_boxes_plasma_tab_shift
    //.byte sine_boxes_plasma_y_tab_shift
    .byte sine_boxes_dynamic_base_tab_shift
    .byte sine_boxes_sprite_offset_x_tab_shift
    .byte sine_boxes_sprite_offset_y_tab_shift
    .byte fld_text_fld_y_tab_shift

    .pc = * "table bias tables"
    .const zoom_plasma_x_offset_bias = 0
    .const zoom_plasma_x_interval_bias = 700
    .const zoom_plasma_x_move_bias = 0
    .const zoom_plasma_x_flow_bias = 0
    .const zoom_plasma_y_offset_bias = 0
    .const zoom_plasma_y_interval_bias = 82
    .const zoom_plasma_y_move_bias = 0
    .const zoom_plasma_y_flow_bias = 0
    .const layers_sin_tab_1_bias = 0
    .const layers_sin_tab_2_bias = 0
    .const sine_boxes_plasma_tab_bias = 2
    .const sine_boxes_dynamic_base_tab_bias = 0
    .const sine_boxes_sprite_offset_x_tab_bias = $ac
    .const sine_boxes_sprite_offset_y_tab_bias = $7e
    .const fld_text_fld_y_tab_bias = 0
tab_bias_low_tab:
    .byte <zoom_plasma_x_offset_bias
    .byte <zoom_plasma_x_interval_bias
    .byte <zoom_plasma_x_move_bias
    .byte <zoom_plasma_x_flow_bias
    .byte <zoom_plasma_y_offset_bias
    .byte <zoom_plasma_y_interval_bias
    .byte <zoom_plasma_y_move_bias
    .byte <zoom_plasma_y_flow_bias
    .byte <layers_sin_tab_1_bias
    .byte <layers_sin_tab_2_bias
    .byte <sine_boxes_plasma_tab_bias
    .byte <sine_boxes_plasma_tab_bias
    .byte <sine_boxes_plasma_tab_bias
    .byte <sine_boxes_plasma_tab_bias
    .byte <sine_boxes_dynamic_base_tab_bias
    .byte <sine_boxes_sprite_offset_x_tab_bias
    .byte <sine_boxes_sprite_offset_y_tab_bias
    .byte <fld_text_fld_y_tab_bias
tab_bias_high_tab:
    .byte >zoom_plasma_x_offset_bias
    .byte >zoom_plasma_x_interval_bias
    .byte >zoom_plasma_x_move_bias
    .byte >zoom_plasma_x_flow_bias
    .byte >zoom_plasma_y_offset_bias
    .byte >zoom_plasma_y_interval_bias
    .byte >zoom_plasma_y_move_bias
    .byte >zoom_plasma_y_flow_bias
    .byte >layers_sin_tab_1_bias
    .byte >layers_sin_tab_2_bias
    .byte >sine_boxes_plasma_tab_bias
    .byte >sine_boxes_plasma_tab_bias
    .byte >sine_boxes_plasma_tab_bias
    .byte >sine_boxes_plasma_tab_bias
    .byte >sine_boxes_dynamic_base_tab_bias
    .byte >sine_boxes_sprite_offset_x_tab_bias
    .byte >sine_boxes_sprite_offset_y_tab_bias
    .byte >fld_text_fld_y_tab_bias

    .pc = * "store_s color table"
store_s_coltab:
    .byte $0e, $03, $0d, $01, $0d, $01, $03, $04, $06, $00

    .pc = $3800 "blank charset placeholder"
    .fill $800, $00
