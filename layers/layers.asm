#import "../common.inc"
#import "layers.inc"
#import "../fld-text/fld-text.inc"

    .const color_mem_shift_speedcode = $6600

    .const color_layer_picture_addr = $7d00

    .const vic_bank_offset = $c000

    .const screen_mem_offset = vic_bank_offset + $2c00
    .const sprite_pointer_offset = screen_mem_offset + $03f8
    .const screen_mem_bits = ((screen_mem_offset - vic_bank_offset) / $400) << 4

    .const sprite_data_block_offset = $0600

    .const zp_base = $02

    .const scroll_text_offset = zp_base
    .const scroll_text_offset_two_frame_toggle_enable = zp_base + 1

    .const scroll_text_block = zp_base + 2

    .const scroll_text_text_offset = zp_base + 3

    .const scroll_text_color_offset = zp_base + 4

    .const draw_next_int_update_mode = zp_base + 6
    .const draw_scroll_text_offset = zp_base + 7
    .const draw_scroll_text_block = zp_base + 8
    .const draw_scroll_text_color_offset = zp_base + 9

    .const char_set_index = zp_base + 10

    .const scroll_y = zp_base + 11
    .const dispatch_color_mem_shift = zp_base + 12

    .const two_frame_toggle = zp_base + 13

    .const color_layer_picture_addr_low = zp_base + 14
    .const color_layer_picture_addr_high = zp_base + 15

    .const dispatch_done = zp_base + 16

    .const temp_char_set_index = zp_base + 17
    .const temp_pixel_y = zp_base + 18
    .const temp_c = zp_base + 19
    .const temp_char_x = zp_base + 20
    .const temp_char_y = zp_base + 21
    .const temp_line = zp_base + 22
    .const temp_i = zp_base + 23
    .const temp_font_char_offset = zp_base + 24

    .const temp_scroll_text_expand_block = zp_base + 17
    .const temp_sprite_y = zp_base + 18

    .pc = layers_code_base "interface"
    jmp init_bg
    jmp init_int

    .pc = * "init bg"
init_bg:
        .pc = * "color mem shift speedcode template"
    // Generate color mem shift speedcode
    lda #<color_mem_shift_speedcode
    sta block_write_addr_low
    lda #>color_mem_shift_speedcode
    sta block_write_addr_high
    ldx #$00
gen_color_mem_shift_speedcode_y:
        txa
        pha

        ldx #$00
gen_color_mem_shift_speedcode_x:
            txa
            pha

            lda #LDA_ABS
            jsr block_write_byte

gen_color_mem_shift_speedcode_load_low_instr:
            lda #40
            jsr block_write_byte

gen_color_mem_shift_speedcode_load_high_instr:
            lda #$d8
            jsr block_write_byte

            lda #STA_ABS
            jsr block_write_byte

gen_color_mem_shift_speedcode_store_low_instr:
            lda #$00
            jsr block_write_byte

gen_color_mem_shift_speedcode_store_high_instr:
            lda #$d8
            jsr block_write_byte

            inc gen_color_mem_shift_speedcode_load_low_instr + 1
            bne !+
                inc gen_color_mem_shift_speedcode_load_high_instr + 1

!:          inc gen_color_mem_shift_speedcode_store_low_instr + 1
            bne !+
                inc gen_color_mem_shift_speedcode_store_high_instr + 1

!:          pla
            tax
        inx
        cpx #40
        bne gen_color_mem_shift_speedcode_x

        pla
        tax
    inx
    cpx #24
    bne gen_color_mem_shift_speedcode_y
    // Write rts at the end
    lda #RTS
    jsr block_write_byte

    // Clear color RAM
    //  We actually write a bit past color RAM here, but that's fine, since next we'll clear the whole bank anyways
    /*lda #$00
    tax
    ldy #$04
clear_color_ram_outer:
clear_color_ram_inner:
clear_color_ram_store_instr:
            sta $d800, x
        inx
        bne clear_color_ram_inner

        inc clear_color_ram_store_instr + 2
    dey
    bne clear_color_ram_outer*/

    // Bank out io regs
    lda #$34
    sta $01

    // Clear entire VIC bank except the last 512 bytes which we won't use
    lda #$00
    tax
    ldy #$3e
clear_vic_bank_outer:
clear_vic_bank_inner:
clear_vic_bank_store_instr:
            sta vic_bank_offset, x
        inx
        bne clear_vic_bank_inner

        inc clear_vic_bank_store_instr + 2
    dey
    bne clear_vic_bank_outer

    // Generate pattern
    lda #>vic_bank_offset
    sta block_write_addr_high

    ldx #$00
gen_char_patterns_char_index:
        txa
        pha

        stx temp_char_set_index

        lda #<vic_bank_offset
        sta block_write_addr_low

        // Small slash pattern (chars 0-15)
        ldx #$00
gen_char_pattern_pixel_y:
            txa
            pha

            sec
            sbc temp_char_set_index
            sec
            sbc temp_char_set_index
            tay

            ldx #$00
gen_char_pattern_pixel_x:
                tya

                lsr
                lsr

                lsr
                rol temp_c

                iny
            inx
            cpx #$08
            bne gen_char_pattern_pixel_x

            lda temp_c
            jsr block_write_byte

            pla
            tax
        inx
        cpx #$08
        bne gen_char_pattern_pixel_y

        lda block_write_addr_high
        clc
        adc #$08
        sta block_write_addr_high

        pla
        tax
    inx
    cpx #$08
    bne gen_char_patterns_char_index

    // Bank in io regs
    lda #$35
    sta $01

    // Generate color layer picture
    lda #<color_layer_picture_addr
    sta block_write_addr_low
    lda #>color_layer_picture_addr
    sta block_write_addr_high

    //  Start with 12 black char lines (2 char rows + spaces between)
    ldx #$00
gen_color_layer_blank_lines_1:
        txa
        pha

        ldx #40
!:          lda #$00
            jsr block_write_byte
        dex
        bne !-

        pla
        tax
    inx
    cpx #12
    bne gen_color_layer_blank_lines_1

    //  Plasma
    ldx #$00
gen_color_layer_picture_plasma_y:
        txa
        pha

        stx temp_char_y

        txa
        asl
        asl
        clc
        adc temp_char_y
        sta gen_color_layer_picture_plasma_add_instr + 1

        ldx #$00
!:          txa
            pha 

            stx temp_char_x

            asl
            asl
            clc
            adc temp_char_x
            tax
            lda layers_sin_tab_1, x
            clc
gen_color_layer_picture_plasma_add_instr:
            adc layers_sin_tab_1
            tax
            lda layers_sin_tab_2, x
            clc
            adc #$08

            tax
            lda color_layer_palette, x
            jsr block_write_byte

            inc gen_color_layer_picture_plasma_add_instr + 1

            pla
            tax
        inx
        cpx #40
        bne !-

        pla
        tax
    inx
    cpx #(24 * 3)
    bne gen_color_layer_picture_plasma_y

    //  End with a full black screen, plus a couple buffer lines
    ldx #$00
gen_color_layer_blank_lines_2:
        txa
        pha

        ldx #40
!:          lda #$00
            jsr block_write_byte
        dex
        bne !-

        pla
        tax
    inx
    cpx #(24 + 2)
    bne gen_color_layer_blank_lines_2

    // Reset color layer pointer
!:  lda #<color_layer_picture_addr
    sta color_layer_picture_addr_low
    lda #>color_layer_picture_addr
    sta color_layer_picture_addr_high

    // Init vars
    lda #$00
    sta scroll_text_offset
    sta scroll_text_block
    sta scroll_text_text_offset
    sta char_set_index
    sta dispatch_color_mem_shift
    sta two_frame_toggle
    sta dispatch_done
    lda #$07
    sta scroll_text_offset_two_frame_toggle_enable
    lda #12
    sta scroll_text_color_offset
    lda #$07
    sta scroll_y

    // Load screen mem pattern data
    lda #<screen_mem_offset
    sta block_write_addr_low
    lda #>screen_mem_offset
    sta block_write_addr_high

    ldx #$00
load_screen_mem_y:
        txa
        pha

        ldx #$00
!:          lda #$00
            jsr block_write_byte
        inx
        cpx #40
        bne !-

        pla
        tax
    inx
    cpx #25
    bne load_screen_mem_y

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

    // Init bg mode
    lda #$10 // Screen enabled, text mode, 24 rows, 0 scroll
    sta $d011

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
    lda #$c4
    sta $dd00

    // Update music
    //inc $d020
    jsr music + 3
    //dec $d020

    // Set up sprite settings
    //  Set up sprite x positions (lower bits)
    lda #(24 + 8)
    ldx #$00
!:      sta $d000, x
        clc
        adc #(8 * 5)
    inx
    inx
    cpx #$10
    bne !-
    //  Set up sprite x positions (higher bits)
    lda #$c0
    sta $d010
    //  Set up sprite color settings
    lda #$ff
    sta $d01c
    //  Set up sprite widths
    lda #$00
    sta $d01d
    //  Set up sprite heights
    lda #$ff
    sta $d017
    //  Enable sprites
    lda #$ff
    sta $d015
    //  Set initial sprite priorities
    lda #$ff
    sta $d01b

    // Set hires mode
    lda $d016
    and #$ef
    sta $d016

    //dec $d020

    // Update
    lda two_frame_toggle
    and #$01
    bne two_frame_toggle_true
        // scroll_y = (scroll_y - 1) & 0x07;
        dec scroll_y
        lda scroll_y
        and #$07
        sta scroll_y
        lda $d011
        and #$78 // Mask out bit 8 of raster pos and scroll bits
        ora scroll_y
        sta $d011

        // char_set_index = (char_set_index + 1) & 0x07;
        //  Note the different bits, as we include a shift in these values here to make the d018 masking simpler
        lda char_set_index
        clc
        adc #$02
        and #$0e
        sta char_set_index

        // if scroll_y == 6
        lda scroll_y
        cmp #$06
        bne !+
            // Dispatch load next color row
            lda #$01
            sta dispatch_color_mem_shift

        // if scroll_y == 7
!:      lda scroll_y
        cmp #$07
        bne !+
            // Dispatch color mem shift
            lda #$02
            sta dispatch_color_mem_shift

        // scroll_text_color_offset -= 1;
!:      dec scroll_text_color_offset

two_frame_toggle_true:
    lda #screen_mem_bits
    ora char_set_index
    sta $d018

    lda scroll_text_offset_two_frame_toggle_enable
    and #$07
    beq scroll_text_offset_update
    lda two_frame_toggle
    and #$01
    beq scroll_text_offset_update
        jmp scroll_text_offset_update_finished
scroll_text_offset_update:
        // if scroll_text_offset < 8 * 5 && (scroll_text_offset & 1) == 0
        lda scroll_text_offset
        cmp #(8 * 5)
        bcs scroll_text_expand_done
        and #$01
        bne scroll_text_expand_done
            // Progressively expand next row of scroll text data into last sprite data row
            //  Bank out io regs
            lda #$34
            sta $01

            //  scroll_text_expand_block = scroll_text_block + 4;
            lda scroll_text_block
            clc
            adc #$04
            //  if scroll_text_expand_block >= 5
            cmp #$05
            bcc !+
                //  scroll_text_expand_block -= 5;
                sec
                sbc #$05
!:          sta temp_scroll_text_expand_block

            //  sprite_y = scroll_text_offset / 2;
            lda scroll_text_offset
            lsr
            sta temp_sprite_y
            //  expand_offset = vic_bank_offset + (0x0800 * scroll_text_expand_block) + SPRITE_DATA_BLOCK_OFFSET + (sprite_y * 3);
            clc
            adc temp_sprite_y
            clc
            adc temp_sprite_y
            sta scroll_text_expand_store_instr + 1
            lda temp_scroll_text_expand_block
            asl
            asl
            asl
            clc
            adc #>sprite_data_block_offset
            adc #>vic_bank_offset
            sta scroll_text_expand_store_instr + 2

            ldx #$00
scroll_text_expand_char:
                txa
                pha

                // char_y = sprite_y / 4;
                lda temp_sprite_y
                lsr
                lsr
                sta temp_char_y

                // font_byte = font[char_y + scroll_text[scroll_text_text_offset + i]];
                txa
                clc
                adc scroll_text_text_offset
                tax
                lda temp_char_y
                clc
                adc scroll_text, x
                tax
                lda font, x

                ldy temp_char_y
                ldx #$00
scroll_text_expand_char_x:
                    pha

                    and #$01
                    beq !+
                        lda sprite_fill_data, y
scroll_text_expand_store_instr:
!:                  sta $abcd, x // Crap addr that will be overwritten

                    pla
                    lsr
                inx
                cpx #$03
                bne scroll_text_expand_char_x

                //  expand_offset += 0x40;
                lda scroll_text_expand_store_instr + 1
                clc
                adc #$40
                sta scroll_text_expand_store_instr + 1
                bcc !+
                    inc scroll_text_expand_store_instr + 2

!:              pla
                tax
            inx
            cpx #$08
            bne scroll_text_expand_char

            //  Bank in io regs
            lda #$35
            sta $01

scroll_text_expand_done:
        // scroll_text_offset += 1;
        inc scroll_text_offset
        // if scroll_text_offset == 8 * 6
        lda scroll_text_offset
        cmp #(8 * 6)
        bne scroll_text_offset_update_finished
            // scroll_text_offset = 0;
            lda #$00
            sta scroll_text_offset

            // Shift rows
            //  scroll_text_block += 1;
            inc scroll_text_block

            //  scroll_text_text_offset += 8;
            lda scroll_text_text_offset
            clc
            adc #$08
            sta scroll_text_text_offset

            //  if scroll_text_block >= 5
            lda scroll_text_block
            cmp #$05
            bne !+
                //  scroll_text_block = 0;
                lda #$00
                sta scroll_text_block
            //  scroll_text_color_offset += 5;
!:          lda scroll_text_color_offset
            clc
            adc #$05
            sta scroll_text_color_offset

scroll_text_offset_update_finished:
    // two_frame_toggle = !two_frame_toggle;
    inc two_frame_toggle

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

        // If we've gone 10 cycles, we're done!
        lda cycle_counter
        cmp #$0a
        bne !+
            inc dispatch_done

!:      inc scroll_text_offset_two_frame_toggle_enable

        // TODO: More stuff based on cycles or something. :D

cycle_update_done:
    // Set up draw vars
    //  draw_scroll_text_offset = 54 - scroll_text_offset;
    lda #54
    sec
    sbc scroll_text_offset
    sta draw_scroll_text_offset
    //  draw_scroll_text_block = scroll_text_block * 32;
    lda scroll_text_block
    asl
    asl
    asl
    asl
    asl
    sta draw_scroll_text_block
    //  draw_scroll_text_color_offset = scroll_text_color_offset;
    lda scroll_text_color_offset
    sta draw_scroll_text_color_offset

    // Dispatch next int or finish
    lda #$00
    sta draw_next_int_update_mode
    jsr dispatch_next_int_or_finish

    // Allow other display interrupts to fire while possibly performing more work
    asl $d019
    cli

    // Check load next color row dispatch
    lda dispatch_color_mem_shift
    cmp #$01
    bne !+
        //dec $d020

        jsr load_next_color_layer_picture_line

        //inc $d020

    // Check color mem shift dispatch
!:  lda dispatch_color_mem_shift
    cmp #$02
    bne !+
        //dec $d020

        jsr color_mem_shift_speedcode

        //inc $d020

!:  lda #$00
    sta dispatch_color_mem_shift

    //inc $d020

    //asl $d019
    pla
    sta $01
    pla
    tay
    pla
    tax
    pla
    rti

    .pc = * "dispatch next int or finish"
dispatch_next_int_or_finish:
    lda draw_next_int_update_mode
    beq dispatch_next_int_or_finish_update_mode_0
        jmp dispatch_next_int_or_finish_update_mode_1

dispatch_next_int_or_finish_update_mode_0:
        lda draw_scroll_text_block
        clc
        adc #(sprite_data_block_offset / 64)
        ldx #$00
!:          sta sprite_pointer_offset, x
            clc
            adc #$01
        inx
        cpx #$08
        bne !-

        lda draw_scroll_text_block
        clc
        adc #32
        cmp #(32 * 5)
        bne !+
            lda #$00
!:      sta draw_scroll_text_block

        lda draw_scroll_text_color_offset
        and #$0f
        tax
        lda scroll_text_colors, x
        sta $d025
        inc draw_scroll_text_color_offset
        lda draw_scroll_text_color_offset
        and #$0f
        tax
        lda scroll_text_colors, x
        inc draw_scroll_text_color_offset
        sta $d026

        lda draw_scroll_text_color_offset
        and #$0f
        tax
        lda scroll_text_colors, x
        inc draw_scroll_text_color_offset
        .for(var i = 0; i < 8; i++) {
            sta $d027 + i
        }

        lda draw_scroll_text_offset
        .for(var i = 0; i < 8; i++) {
            sta $d001 + i * 2
        }
    jmp dispatch_next_int_or_finish_dispatch

dispatch_next_int_or_finish_update_mode_1:
        lda draw_scroll_text_color_offset
        and #$0f
        tax
        lda scroll_text_colors, x
        sta $d025
        inc draw_scroll_text_color_offset
        lda draw_scroll_text_color_offset
        and #$0f
        tax
        lda scroll_text_colors, x
        inc draw_scroll_text_color_offset
        sta $d026

dispatch_next_int_or_finish_dispatch:
    lda draw_scroll_text_offset
    clc
    adc #(8 * 3)
    // If we overflow from the previous add, early out, since the raster compare will be meaningless
    bcs dispatch_next_int_or_finish_finished

    sta draw_scroll_text_offset

    cmp #246
    bcs dispatch_next_int_or_finish_finished
        // We'll want to update the positions at the end of the current row, which is 8 pixels above the start of the next one
        sec
        sbc #$08
        sta $d012
        lda #<update_sprite_y_positions_int
        sta $fffe
        lda #>update_sprite_y_positions_int
        sta $ffff
    jmp dispatch_next_int_or_finish_ret

dispatch_next_int_or_finish_finished:
        lda #<frame
        sta $fffe
        lda #>frame
        sta $ffff
        lda #$ff
        sta $d012

        // If we're done, switch to text instead
        lda dispatch_done
        beq dispatch_next_int_or_finish_ret
            jsr fld_text_init_int

dispatch_next_int_or_finish_ret:
    lda draw_next_int_update_mode
    eor #$01
    sta draw_next_int_update_mode

    rts

    .pc = * "update sprite y positions int"
update_sprite_y_positions_int:
    pha
    txa
    pha
    tya
    pha
    lda $01
    pha

    //inc $d020

    jsr dispatch_next_int_or_finish

    //dec $d020

    asl $d019
    pla
    sta $01
    pla
    tay
    pla
    tax
    pla
    rti

    .pc = * "load next color layer picture line"
load_next_color_layer_picture_line:
    ldy #$00
!:      lda (color_layer_picture_addr_low), y
        sta $d800 + 24 * 40, y
    iny
    cpy #40
    bne !-

    lda color_layer_picture_addr_low
    clc
    adc #40
    sta color_layer_picture_addr_low
    bcc !+
        inc color_layer_picture_addr_high

!:  rts

    .file [name="layers-data.prg", segments="Data"]
    .segmentdef Data [start=layers_data_base]
    .segment Data "data"

    .pc = * "font"
font:
    .import binary "font.bin"

    .pc = * "scroll text colors"
scroll_text_colors:
    .import binary "scroll-text-colors.bin"

    .pc = * "sprite fill data"
sprite_fill_data:
    .byte $55
    .byte $ff
    .byte $aa
    .byte $55
    .byte $ff

    .pc = * "color layer palette"
color_layer_palette:
    .import binary "color-layer-palette.bin"

    .align $100
    .pc = * "scroll text"
scroll_text:
    .import binary "scroll-text.bin"
