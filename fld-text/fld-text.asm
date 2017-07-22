#import "../common.inc"
#import "fld-text.inc"
#import "../zoom-plasma/zoom-plasma.inc"

    .const zp_base = $80

    .const fld_y_index = zp_base
    .const fld_y = zp_base + 1

    .const enable_motion = zp_base + 3

    .pc = fld_text_code_base "interface"
    jmp init_bg
    jmp init_int

    .pc = * "init bg"
    // Enables/disables motion based on value of a
init_bg:
    sta enable_motion

    // Clear screen
    ldx #$00
    lda #$20
!:      sta $0400, x
        sta $0500, x
        sta $0600, x
        sta $0700, x
    dex
    bne !-

    // Init vars
    lda #$30
    sta fld_y_index

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
    sta cycle_frame_counter
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

    // Reset VIC bank
    lda #$c7
    sta $dd00

    // Set default screen location, lowercase chars
    lda #$16
    sta $d018

    // Disable sprites
    lda #$00
    sta $d015

    // Update music
    //inc $d020
    jsr music + 3
    //dec $d020

    // Copy text
    ldx #$00
copy_text_read_instr:
!:      lda text, x
        sta $0400, x
    inx
    cpx #40
    bne !-

    // Fld
    inc fld_y_index
    inc fld_y_index

    ldx fld_y_index
    lda #144
    clc
    adc fld_text_fld_y_tab, x
    sta fld_y

    lda enable_motion
    bne fld_wait_1
        lda #144
        sta fld_y

fld_wait_1:
        lda $d012
!:          cmp $d012
        beq !-
        cmp fld_y
        beq !+
            and #$07
            ora #$18
            sta $d011
    jmp fld_wait_1

    // Set background colors for text (after waiting a bit..)
!:  ldx #$07
!:      dex
    bne !-
    nop
    lda cycle_frame_counter
    lsr
    tax
    lda color_ramp, x
    sta $d020
    sta $d021

    // Wait a few lines, then change colors back
    lda fld_y
    clc
    adc #$12
    sta fld_y

fld_wait_2:
        lda $d012
!:          cmp $d012
        beq !-
    cmp fld_y
    bne fld_wait_2

    ldx #$09
!:      dex
    bne !-
    nop
    nop

    lda #$00
    sta $d020
    sta $d021

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

        // If we've gone 1 cycle with motion disabled, then we're basically at the end. Shut it down and return to BASIC
        lda enable_motion
        bne !+
        lda cycle_counter
        cmp #$01
        bne !+
            // Turn off screen so we don't glitch or anything
            lda #$00
            sta $d011

            // Bank in KERNAL and BASIC
            lda #$37
            sta $01

            // ACK interrupts
            /*lda #$01
            sta $dd0d
            lda $dd0d*/
            asl $d019

            // And we're out!
            jmp ($fffc)

        // If we've gone 7 cycles, disable motion
!:      lda cycle_counter
        cmp #$07
        bne !+
            lda #$00
            sta enable_motion

        // If we've gone 8 cycles, time to fire up the next effect
!:      lda cycle_counter
        cmp #$08
        bne !+
            jsr zoom_plasma_init_int

        // Increment text read pointer
!:      lda copy_text_read_instr + 1
        clc
        adc #40
        sta copy_text_read_instr + 1
        bcc !+
            inc copy_text_read_instr + 2

        // Offset y tab indices (looks a bit nicer)
!:      lda fld_y_index
        clc
        adc #$37
        sta fld_y_index

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

    .file [name="fld-text-data.prg", segments="Data"]
    .segmentdef Data [start=fld_text_data_base]
    .segment Data "data"

    .pc = * "color ramp"
color_ramp:
    .byte $00, $06, $04, $03, $01, $0d, $0e, $04
    .for(var i = 0; i < cycle_frames / 2 - 16; i++) {
        .byte $04
    }
    .byte $04, $0e, $0d, $01, $03, $04, $06, $00

    .pc = * "text"
text:
    //     ----------------------------------------
    .text "                logicoma                "
    .text "            at solskogen 2017           "
    .text "                presents                "
    .text "               a c64 demo               "
    .text "                 in 4kb                 "
    .text "                  this                  "
    .text "                   is                   "
    .text "               makeshift.               "

    .text "             logicoma  2017             "
