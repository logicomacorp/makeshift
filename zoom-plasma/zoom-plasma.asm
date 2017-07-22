#import "../common.inc"
#import "zoom-plasma.inc"
#import "../sine-boxes/sine-boxes.inc"

    .const stretching_loop_speedcode = $6600

    .const zp_base = $02

    .const scale_index = zp_base
    .const x_move_index = zp_base + 1
    .const y_move_index = zp_base + 2
    .const y_flow_index = zp_base + 3

    .const x_interval_low = zp_base + 4
    .const x_interval_high = zp_base + 5
    .const char_x_low = zp_base + 6
    .const char_x_high = zp_base + 7

    .const y_interval_low = zp_base + 8
    .const y_interval_high = zp_base + 9
    .const line_y = zp_base + 10
    .const line_y_low = line_y
    .const line_y_high = zp_base + 11

    .const char_data_last_byte = zp_base + 12

    .const x_flow_index = zp_base + 13

    .const calc_line_temp = zp_base + 14

    .const x_flow_index_temp = zp_base + 15

    .pc = zoom_plasma_code_base "interface"
    jmp init_bg
    jmp init_int

    .pc = * "init bg"
init_bg:
    // Init variables
    lda #74
    sta scale_index
    lda #$01
    sta scale_index_inc_instr + 1
    lda #20
    sta x_move_index
    lda #$01
    sta x_move_index_inc_instr + 1
    lda #80
    sta y_move_index
    lda #$01
    sta y_move_index_inc_instr + 1
    lda #0
    sta y_flow_index
    lda #$20
    sta x_flow_index

    // Unpack chars
    // Bank out io regs
    lda #$34
    sta $01

    // Set up read pointer
    lda #<chars
    sta block_read_addr_low
    lda #>chars
    sta block_read_addr_high
    // Set up write pointer
    lda #$00
    sta block_write_addr_low
    lda #$c0
    sta block_write_addr_high

    // Unpacks a single char and updates read/write pointer. Clobbers a & y
    .macro UnpackChar() {
        ldy #$00

        // Read character line byte
        lda (block_read_addr), y

        // Write byte 8 times, expanding into a character
!:          sta (block_write_addr), y
        iny
        cpy #$08
        bne !-

        // Adjust read pointer
        lda #$01
        jsr block_inc_read_addr

        // Adjust write pointer by 8 bytes (move to next output char)
        lda #$08
        jsr block_inc_write_addr
    }

    // Loop through 7 256 byte blocks
    ldy #$07
unpack_block_loop:
        tya
        pha

        ldx #$00
unpack_chars_loop:
            UnpackChar()
        dex
        bne unpack_chars_loop

        pla
        tay
    dey
    bne unpack_block_loop
    // Unpack last block _except_ for the last char so we don't overwrite the irq vector
    ldx #$ff
unpack_chars_last_block_loop:
        UnpackChar()
    dex
    bne unpack_chars_last_block_loop
    // Unpack first 6 bytes of last char and store the value into char_data_last_byte
    //  so we can fill in the last two bytes over the irq vector for rendering later,
    //  but not clobber it here
    ldy #$00
    lda (block_read_addr), y
!:      sta (block_write_addr), y
    iny
    cpy #$06
    bne !-
    sta char_data_last_byte

    // Bank in io regs
    lda #$35
    sta $01

    // Generate stretching loop speedcode
    lda #<stretching_loop_speedcode
    sta block_write_addr_low
    lda #>stretching_loop_speedcode
    sta block_write_addr_high
    ldx #$00
!:      txa
        pha

        // Write d011 value in template
        //  $18 | ((i + 2) & $07)
        clc
        adc #$02
        and #$07
        ora #$18
        sta stretching_loop_speedcode_template_update_instr + 1

        // Copy template
        lda #<stretching_loop_speedcode_template_start
        sta block_read_addr_low
        lda #>stretching_loop_speedcode_template_start
        sta block_read_addr_high
        lda #(stretching_loop_speedcode_template_end - stretching_loop_speedcode_template_start)
        jsr block_copy_bytes

        pla
        tax
    inx
    cpx #198
    bne !-
    // Write rts at the end
    lda #RTS
    jsr block_write_byte

    // Make charset at $3800 is empty
    //  Currently we have a placeholder there so we can skip this
    //  TODO: If we put this back in, do smc loops instead of unrolling
    /*lda #$00
    tax
!:      .for(var i = 0; i < 8; i++) {
            sta $3800 + i * $100
        }
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

    // Set multicolor text mode
    lda $d016
    ora #$10
    sta $d016

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

    // Reset screen mem and point char data at empty chars ($3800)
    lda #$1e
    sta $d018

    // Reset VIC bank
    lda #$c7
    sta $dd00

    // Update music
    //inc $d020
    jsr music + 3
    //dec $d020

    // Set up colors
    lda #$03
    sta background_color_instr + 1

    lda #$0e
    sta $d022

    lda cycle_counter
    lsr
    and #$01
    tax

    lda screen_dark_colors, x
    sta $d023

    lda char_colors, x
    ldx #$28
!:      sta $d800 - 1, x
    dex
    bne !-

    // Flow scripting
    lda cycle_counter
    lsr
    tax

    lda x_flow_index_inc_tab, x
    sta x_flow_index_inc_instr + 1

    lda y_flow_index_inc_tab, x
    sta y_flow_index_inc_instr + 1

    // Update effect indices
    lda scale_index
    clc
scale_index_inc_instr:
    adc #$00
    sta scale_index
    lda x_move_index
    clc
x_move_index_inc_instr:
    adc #$00
    sta x_move_index
    lda y_move_index
    clc
y_move_index_inc_instr:
    adc #$00
    sta y_move_index
    lda y_flow_index
    clc
y_flow_index_inc_instr:
    adc #$00
    sta y_flow_index
    lda x_flow_index
    clc
x_flow_index_inc_instr:
    adc #$00
    sta x_flow_index

    // Update char map
    //dec $d020

    // Load x vars
scale_index_instr:
    ldx scale_index
    ldy x_move_index
    // x_offset = char_x = x_offset_tab[scale_index] + x_move_tab[x_move_index];
    lda zoom_plasma_x_offset_low_tab, x
    clc
    adc zoom_plasma_x_move_low_tab, y
    sta char_x_low
    lda zoom_plasma_x_offset_high_tab, x
    adc zoom_plasma_x_move_high_tab, y
    sta char_x_high
    // x_interval = x_interval_tab[scale_index];
    lda zoom_plasma_x_interval_low_tab, x
    sta x_interval_low
    lda zoom_plasma_x_interval_high_tab, x
    sta x_interval_high

    // X update
    lda char_x_high

    ldx #$00
!:      stx x_flow_index_temp

        pha
        pha
        clc
        adc x_flow_index
        tax
        lda char_x_low
        asl
        pla
        rol
        clc
        adc zoom_plasma_x_flow_tab, x

        ldx x_flow_index_temp
        sta $400, x

        // Technically we don't need to do this on the last iteration, but it simplifies the code to leave it in
        lda char_x_low
        clc
        adc x_interval_low
        sta char_x_low
        pla
        adc x_interval_high

        ldx x_flow_index_temp
    inx
    cpx #40
    bne !-

    //inc $d020

    // Load y vars
    ldx scale_index
    // y_interval = y_interval_tab[scale_index];
    lda zoom_plasma_y_interval_low_tab, x
    sta y_interval_low
    lda zoom_plasma_y_interval_high_tab, x
    sta y_interval_high
    // line_y = y_offset_tab[scale_index] + y_move_tab[y_move_index];
    ldy y_move_index
    lda zoom_plasma_y_offset_low_tab, x
    clc
    adc zoom_plasma_y_move_low_tab, y
    sta line_y_low
    lda zoom_plasma_y_offset_high_tab, x
    adc zoom_plasma_y_move_high_tab, y
    sta line_y_high

    // Assumes high byte of line_y is in a and x, outputs shifted (NOT MASKED) line index in a (clobbering y)
    .macro CalcLine() {
        sta calc_line_temp

        clc
        adc y_flow_index
        asl // Instead of doubling y_flow_tab's phase, we can just shift here
        tay
        lda zoom_plasma_y_flow_tab, y

        clc
        adc calc_line_temp

        nop
        nop
        nop
        nop
        nop
        nop
        nop
    }

    // Inputs high byte of line_y in x, outputs updated high byte of line_y in a and x
    .macro UpdateLine() {
        lda line_y_low
        clc
        adc y_interval_low
        sta line_y_low
        txa
        adc y_interval_high
        tax
    }

    // Calc d018 value for first plasma line
    lax line_y_high
    CalcLine()
    sta first_d018_value_instr + 1
    UpdateLine()
    stx line_y_high

    // Set up stretching raster interrupt
    lda #<stable_raster_a
    sta $fffe
    lda #>stable_raster_a
    sta $ffff
    lda #$2b
    sta $d012

!:  asl $d019
    pla
    sta $01
    pla
    tay
    pla
    tax
    pla
    rti

    .align $100
    .pc = * "stable raster a"
stable_raster_a:
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

    // Wait a bit for the next scanline
    ldx #$0a
!:      dex
    bne !-
    bit $00

    // Set up next interrupt stage
    lda #<stable_raster_b
    sta $fffe
    inc $d012

    // ACK so next stage can fire
    asl $d019

    // Save sp into x (we'll restore in the next stage)
    tsx

    // Clear interrupt flag so next stage can fire
    cli

    // nop pads (next stage should fire in here somewhere)
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    jmp * // Safety net (if we see more than 1-cycle jitter for the next int, we got here)

    // Technically this is only semi-stable with a 1-cycle jitter (close enough for this effect)
    .pc = * "stable raster b"
stable_raster_b:
    // Top border, thin white part
    lda #$01
    sta $d021
    lda #$00
    // Make sure there's no black crap on the border
    sta $ffff

    // Restore sp
    txs

    // Wait a bit...
    ldx #$17
!:      dex
    bne !-

    // Set up screen to read screen/color mem from current bank before stretching loop
    lda #$19
    sta $d011

    // Wait a bit more..
    ldx #$14
!:      dex
    bne !-
    nop

    // Write last byte of char data over interrupt vector (lol)
    lda char_data_last_byte
    sta $fffe
    sta $ffff

    // Set plasma background color
background_color_instr:
    lda #$00
    sta $d021

    // Set first scanline d018 value
first_d018_value_instr:
    lda #$00
    sta $d018

    // Switch VIC bank
    //  We want to do this pretty late so that the screen and color mem will have already been read for the current char row,
    //  but not so late that we show any wrong pixels at the beginning of the first line
    lda #$c4
    sta $dd00

    // Preload line index for stretching loop
    lax line_y + 1
    // Stretching loop (should start at line 50 cycle 1/2)
    jsr stretching_loop_speedcode

    // Reset background color
    //  Need to wait until the last char row has been displayed so we don't mess up its background color
    ldx #$07
!:      dex
    bne !-
    lda #$00
    sta $d021
    // Make sure there's no black crap on the border
    sta $ffff

    // Reset VIC bank
    lda #$c7
    sta $dd00

    // Set up frame interrupt
    lda #<frame
    sta $fffe
    lda #>frame
    sta $ffff
    lda #$ff
    sta $d012

    // Update cycle
!:  inc cycle_frame_counter
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
            jsr sine_boxes_init_int

        // Script movement changes
!:      lda cycle_counter
        and #$01
        bne cycle_update_done

            lda x_move_index
            clc
            adc #$63
            sta x_move_index

            lda y_move_index
            clc
            adc #$b9
            sta y_move_index

            lda scale_index
            clc
            adc #$63
            sta scale_index

            dec scale_index_inc_instr + 1

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

    .pc = * "stretching loop speedcode template"
    // This should be repeated 198 times (one for each scanline the effect runs, minus the first line)
stretching_loop_speedcode_template_start:
    CalcLine()

    // Stretch
stretching_loop_speedcode_template_update_instr:
    ldy #00 // $18 | ((i + 2) & $07)
    sty $d011 // This write should happen on cycle 55-57 each line
    sta $d018 // Select charset

    // Technically we don't need to do this on the last iteration, but it simplifies speedcode to leave it in
    UpdateLine()
stretching_loop_speedcode_template_end:

    .file [name="zoom-plasma-data.prg", segments="Data"]
    .segmentdef Data [start=zoom_plasma_data_base]
    .segment Data "data"

    .pc = * "chars"
chars:
    .import binary "chars.bin"

    /*.pc = * "alt chars"
alt_chars:
    .import binary "alt-chars.bin"*/

    .pc = * "char colors"
char_colors:
    .byte $0a, $08

    .pc = * "screen dark colors"
screen_dark_colors:
    .byte $04, $06

    .pc = * "x flow index inc table"
x_flow_index_inc_tab:
    .byte $ff, $04, $00, $fd

    .pc = * "y flow index inc table"
y_flow_index_inc_tab:
    .byte $01, $ff, $00, $02
