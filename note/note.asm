    .pc = $0801 "autostart"
    :BasicUpstart(entry)

    .pc = $080d "entry"
entry:
    sei

    // Bank out BASIC and kernal ROMs
    lda #$35
    sta $01

    jsr mask_nmi

    // Turn off CIA interrupts
    lda #$7f
    sta $dc0d
    sta $dd0d

    // Enable raster interrupts
    lda #$01
    sta $d01a

    jsr init

    // Ack CIA interrupts
    lda $dc0d
    lda $dd0d

    // Ack VIC interrupts
    asl $d019

    cli

    jmp bg_thread

    .pc = * "mask nmi"
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
    lda #<nmi
    sta $fffa
    lda #>nmi
    sta $fffb

    // Start timer A (NMI triggers immediately)
    lda #$01
    sta $dd0e

    rts

nmi:
    rti

    .const state_loading = $00
    .const state_display = $01
    .const state_mask = $01

    .const bg_color_1 = $06
    .const bg_color_2 = $04
    .const border_color = $00

    .const zp_base = $02

    .const state = zp_base

    .const text_load_active = zp_base + 1

    .const text_read_ptr_low = zp_base + 2
    .const text_read_ptr_high = zp_base + 3

    .const text_write_ptr_low = zp_base + 4
    .const text_write_ptr_high = zp_base + 5

    .const text_color_index = zp_base + 6

    .pc = * "init"
init:
    // Set screen mode
    lda #$1b
    sta $d011

    // Set charset ptr
    lda #$1a
    sta $d018

    // Set up vars
    lda #state_loading
    sta state
    lda #$01
    sta text_load_active
    lda #$00
    sta text_color_index

    // Load initial text colors
    lda #bg_color_2
    ldx #$00
!:      sta $d800, x
        sta $d900, x
        sta $da00, x
        sta $db00, x
    inx
    bne !-

    // Set up frame interrupt
    lda #<frame
    sta $fffe
    lda #>frame
    sta $ffff
    lda #$00
    sta $d012

    // Set up music
    lda #$00
    tax
    tay
    jsr music

    rts

    .pc = * "bg thread"
bg_thread:
    lda #<text_start
    sta text_read_ptr_low
    lda #>text_start
    sta text_read_ptr_high

bg_thread_main_loop:
    lda text_load_active
    beq bg_thread_main_loop

    // Load text
    lda #<$400
    sta text_write_ptr_low
    lda #>$400
    sta text_write_ptr_high

    ldx #$00
load_screen_y:
        ldy #$00
!:          lda (text_read_ptr_low), y
            sta (text_write_ptr_low), y
        iny
        cpy #40
        bne !-

        lda text_read_ptr_low
        clc
        adc #40
        sta text_read_ptr_low
        bcc !+
            inc text_read_ptr_high

!:      lda text_write_ptr_low
        clc
        adc #40
        sta text_write_ptr_low
        bcc !+
            inc text_write_ptr_high
!:  inx
    cpx #25
    bne load_screen_y

    lda text_read_ptr_low
    cmp #<text_end
    bne !+
    lda text_read_ptr_high
    cmp #>text_end
    bne !+
        lda #<text_start
        sta text_read_ptr_low
        lda #>text_start
        sta text_read_ptr_high

!:  lda #$00
    sta text_load_active

    jmp bg_thread_main_loop

    .pc = * "frame"
frame:
    pha
    txa
    pha
    tya
    pha

    // Update music
    //inc $d020
    jsr music + 3
    //dec $d020

    // Set initial bg colors
    lda #bg_color_1
    sta $d020
    lda #bg_color_2
    sta $d021

    // Set up first border interrupt
    lda #<border1
    sta $fffe
    lda #>border1
    sta $ffff
    lda #44
    sta $d012

    // Let border ints fire while filling in text color
    asl $d019
    cli

    lda state
    beq loading
    jmp display

loading:
        // Fade out text color
        ldx text_color_index
        beq loading_check_transition
            dex
            stx text_color_index

            // If text_color transitions to zero, dispatch text loading
            bne !+
                inc text_load_active

!:          jmp state_done

        // Wait for text_color_index and text_load_active to be 0
loading_check_transition:
        lda text_load_active
        bne !+
            // Switch states
            lda #state_display
            sta state

!:      jmp state_done

display:
        // Fade in text color
        ldx text_color_index
        cpx #$09
        beq !+
            inx
            stx text_color_index
            jmp state_done

        // Wait for text_color_index to be 0 and space bar to be pressed
        //  Port a data dir (output)
!:      lda #$ff
        sta $dc02
        //  Port b data dir (input)
        lda #$00
        sta $dc03
        //  Output row on port a
        lda #$7f
        sta $dc00
        //  Input column from port b
        lda $dc01
        //  Check for space
        //   TODO: I actually don't know why this is the particular bitmask we need, but it works just fine for this :)
        and #$10
        bne state_done
            // Switch states
            lda #state_loading
            sta state

state_done:
    // Text color
    //inc $d020

    ldx text_color_index
    lda text_color_ramp, x
    ldx #$00
!:      sta $d800, x
        sta $d900, x
        sta $da00, x
        sta $db00, x
    inx
    bne !-

    //dec $d020

    pla
    tay
    pla
    tax
    pla
    asl $d019
    rti

    .pc = * "border 1"
border1:
    pha
    txa
    pha
    tya
    pha

    // Wait a bit
    ldx #$06
!:      dex
    bne !-
    nop
    nop

    // Border
    lda #border_color
    sta $d020

    // Wait a bit more :)
    ldx #$17
!:      dex
    bne !-
    nop
    nop

    // Set background color
    lda #bg_color_2
    sta $d020

    // Set up second border interrupt
    lda #<border2
    sta $fffe
    lda #>border2
    sta $ffff
    lda #254
    sta $d012

    pla
    tay
    pla
    tax
    pla
    asl $d019
    rti

    .pc = * "border 2"
border2:
    pha
    txa
    pha
    tya
    pha

    // Wait a bit
    ldx #$06
!:      dex
    bne !-
    nop
    nop

    // Border
    lda #border_color
    sta $d020

    // Wait a bit more :)
    ldx #$17
!:      dex
    bne !-
    nop
    nop

    // Reset background color
    lda #bg_color_1
    sta $d020

    // Set up frame interrupt
    lda #<frame
    sta $fffe
    lda #>frame
    sta $ffff
    lda #$00
    sta $d012

    pla
    tay
    pla
    tax
    pla
    asl $d019
    rti

    .pc = $1000 "music"
music:
    .import c64 "sourceflux.prg"

    .pc = $2800 "charset"
charset:
    .import binary "quadrotext.bin"

    .pc = * "text color ramp"
text_color_ramp:
    .byte $04, $04, $0c, $0c, $03, $03, $0d, $0d, $01, $01

    .pc = * "text"
text_start:
    .encoding "screencode_mixed"

    //    "----------------------------------------"
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "               makeshift                "
    .text "         a demo in 4 kilobytes          "
    .text "              by logicoma               "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "              Code: ferris              "
    .text "          Graphics: ferris, wobble      "
    .text "             Music: h0ffman             "
    .text "      Sound driver: 4mat                "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "                                        "
    .text "      press spacebar to read on...      "
    .text "                                        "
    .text "                                        "
    .text "                                        "

    .text "NOTES ON VISUALS ETC - FERRIS           "
    .text "                                        "
    .text "Eyoo, ferris here! Finally sitting down "
    .text "to write this thing.. It's been a week  "
    .text "or so since the demo was released, and I"
    .text "meant to get this out sooner, but you   "
    .text "know - real life and whatnot :) ANYWAYS."
    .text "                                        "
    .text "Phew, what a little journey this prod   "
    .text "became! It started out with some reverse"
    .text "engineering of the plasmas in Edge of   "
    .text "Disgrace, simply to wrap my head around "
    .text "some of the hardware tricks used on C64 "
    .text "in a 'real' demo context, followed by me"
    .text "realizing this same technique could be  "
    .text "extended to zooming, followed by a bunch"
    .text "of unrelated projects, followed by me   "
    .text "realizing I hadn't done a 4k in years   "
    .text "and that the only idea I had laying     "
    .text "around was the zoomplasma, followed by  "
    .text "me making it work on the C64, at which  "
    .text "point I knew we had to do a 4k for sol- "
    .text "skogen :) . Originally it was going to  "
    .text "be just that effect and the simple text "
    .text "stuff, with a simple music driver occu- "

    .text "pying around 2.5kb or so, with a couple "
    .text "variations on the effect and a custom   "
    .text "packer (as I was already playing with   "
    .text "our 64k pc packer at the time, and com- "
    .text "pression is basically my crack) However,"
    .text "as h0ff looked into the tools available "
    .text "for music, he ended up contacting 4mat, "
    .text "who sent us his 1k play driver and      "
    .text "sample tune, which only occupied around "
    .text "1kb for the entire thing.. so then I    "
    .text "knew I had to stuff more effects in     "
    .text "here, which I was honestly pretty happy "
    .text "about, since I really like this shit for"
    .text "some reason..                           "
    .text "                                        "
    .text "So yeah, I guess we can start with the  "
    .text "effects in the order that I made them   "
    .text "in (roughly):                           "
    .text "                                        "
    .text "ZOOMPLASMA                              "
    .text "==========                              "
    .text "                                        "
    .text "As mentioned earlier, I wanted to get a "
    .text "better grasp on the various hardware    "
    .text "tricks that are used on C64. In parti-  "

    .text "cular, FPP always interested me due to  "
    .text "its inherent flexibility and how smooth "
    .text "effects produced with it tended to look."
    .text "The last time I had really look into any"
    .text "C64 stuff at all was around 2011 - I    "
    .text "tend to dig in a bit every couple years,"
    .text "learn some stuff, make a new effect, and"
    .text "then put it down for awhile - and I had "
    .text "actually taken some VICE snapshots of   "
    .text "some of the Edge of Disgrace effects at "
    .text "that time, and still had them laying    "
    .text "around. Since I knew the plasmas in the "
    .text "beginning of the demo were done using   "
    .text "FPP, I thought digging into those would "
    .text "be a good place to start.               "
    .text "                                        "
    .text "I sat down for several evenings in the  "
    .text "VICE monitor with the effects (this was "
    .text "before I knew about the fantastic new   "
    .text "C64 Debugger the Samar guys are putting "
    .text "out recently, which REALLY would've     "
    .text "helped, but oh well!) and started taking"
    .text "as many notes as I could about how it   "
    .text "worked. I'm no C64 expert by any means, "
    .text "so it was hard to discern what was rele-"

    .text "vant and what wasn't. So, I took as many"
    .text "detailed notes as I could! I did this in"
    .text "the open btw, you can find all of those "
    .text "notes (and any future ones I might take)"
    .text "on my github:                           "
    .text "                                        "
    .text "github.com/yupferris/rip-and-tear       "
    .text "                                        "
    .text "Anyways, some key points about how the  "
    .text "original works (ignoring the sprite     "
    .text "zoomer on top, which really makes these "
    .text "screens pretty rad, but that's for ano- "
    .text "ther time, hehe):                       "
    .text "                                        "
    .text " - The main plasma is displayed using   "
    .text "   char-mode FPP, where the last line of"
    .text "   each character of the first screen   "
    .text "   row is stretched, and changes to the "
    .text "   current charset pointer ($d018) will "
    .text "   select which line of graphics will   "
    .text "   actually be displayed every scanline."
    .text "   This allows for ample raster time per"
    .text "   line (no badlines), while limiting   "
    .text "   the amount of lines to choose from to"
    .text "   8 per bank (and there's only one bank"

    .text "   used here). But, with a plasma like  "
    .text "   this, that turns out to be enough :) "
    .text "                                        "
    .text " - Unlike typical FPP-plasmas, where the"
    .text "   plasma only flows in the y-direction,"
    .text "   this plasma also flows in the x-dir- "
    .text "   ection. To do this, the character    "
    .text "   indices for the first row of chars   "
    .text "   (the row that gets stretched) is up- "
    .text "   dated each frame. This effectively   "
    .text "   changes the 'base picture' that gets "
    .text "   stretched with a sinus distortion in "
    .text "   the y-direction by the FPP routine   "
    .text "   each frame, so the plasma is much    "
    .text "   more dynamic and authentic. And it's "
    .text "   also cheap - since the charset is    "
    .text "   totally precalculated, it's only a   "
    .text "   matter of changing the 40 (or in the "
    .text "   case of EoD, 38 because of the static"
    .text "   border) char indices that make up the"
    .text "   first char line. Nice! :)            "
    .text "                                        "
    .text " - The screen mem that contains the char"
    .text "   indices is actually stored in a diff-"
    .text "   erent bank than the char data! This  "

    .text "   one took me a bit to figure out, esp."
    .text "   since the EoD implementation never   "
    .text "   changes the bank via $dd00 - rather, "
    .text "   $dd02 (the port direction for $dd00) "
    .text "   is changes instead, which will have  "
    .text "   the same result, but is a bit more   "
    .text "   indirect (likely to play nicely with "
    .text "   disk loading or something). This     "
    .text "   works because the stretching trick is"
    .text "   really just adjusting the internal   "
    .text "   VIC counters to never read any more  "
    .text "   data into its internal screen mem    "
    .text "   buffer, so once the hw has loaded the"
    .text "   char and color mem for the first line"
    .text "   we can safely switch banks for the   "
    .text "   stretcher and we're good. The benefit"
    .text "   here is that you can take up the     "
    .text "   entire bank with char data instead of"
    .text "   having to squeeze in the screen mem  "
    .text "   as well, so that's nice.             "
    .text "                                        "
    .text "I'm skipping a few details here, but the"
    .text "astute reader will have gleaned enough  "
    .text "from these points to work out the gist  "
    .text "of the routine. Each frame, we're going "

    .text "to update the char indices of the first "
    .text "graphics row, then get the hardware to  "
    .text "display that row over and over again,   "
    .text "but we'll swap out which character      "
    .text "graphics it displays each scanline. This"
    .text "way we get the x-flow in the form of    "
    .text "char index updates, and the y-flow in   "
    .text "the form of character set selection as  "
    .text "we stretch the display.                 "
    .text "                                        "
    .text "From there, the extension to a zooming  "
    .text "plasma is actually quite trivial. Both  "
    .text "of the flow loops (char selection and   "
    .text "charset selection) take the form of a   "
    .text "running sum with a sine table offset,   "
    .text "which gets looked up in a table and     "
    .text "stored (either to screen mem or to      "
    .text "$d018). If we do these running sums in  "
    .text "16-bit rather than 8-bit (which we have "
    .text "plenty of raster time to do in both     "
    .text "cases), we suddenly have enough fract-  "
    .text "ional bits to represent quite smooth    "
    .text "changes in our running sums, all without"
    .text "ever having to perform shifts (we can   "
    .text "just take the high byte of the 16-bit   "

    .text "values). Combine that with some tables  "
    .text "for offsets where our sums start and the"
    .text "delta values we add in our running sums,"
    .text "and voila - we have zooming! It's really"
    .text "that simple.                            "
    .text "                                        "
    .text "Surprisingly, I have yet to see this    "
    .text "actually done on C64, which is one of   "
    .text "the reasons I was pretty excited about  "
    .text "it. It's a pretty minute difference to  "
    .text "what's already out there, but a pretty  "
    .text "cool-looking one too if I do say so     "
    .text "myself :) . And to be fair I HAVE seen  "
    .text "another plasma that zooms in x, and I   "
    .text "wouldn't be at all surprised if I missed"
    .text "an earlier implementation of this that  "
    .text "covers the full char area here. But, as "
    .text "far as I can tell, I'm the first - so   "
    .text "I'll enjoy that until someone can give  "
    .text "me a little history lesssion, or not ;)."
    .text "                                        "
    .text "Since I hadn't really done a char plasma"
    .text "before, it took me many iterations to   "
    .text "find out what the chars should look like"
    .text "and which parts would be baked into the "

    .text "charset. Luckily all of this can be     "
    .text "easily prototyped. I went through a few "
    .text "different iterations of different baked "
    .text "charsets, different combinations of sin "
    .text "in both x and y (some of which were     "
    .text "quite difficult do display given the    "
    .text "number of cycles per scanline), and     "
    .text "eventually landed on what's there curr- "
    .text "ently, which is pretty cheap and simple."
    .text "For a simple math demonstration, I      "
    .text "recommend checking out this shader I    "
    .text "made in shadertoy:                      "
    .text "                                        "
    .text "https://www.shadertoy.com/view/XsBBDm   "
    .text "                                        "
    .text "It was made after the intro, so it won't"
    .text "match exactly (and it doesn't zoom), but"
    .text "if you were ever curious about the math "
    .text "here, that should help :) .             "
    .text "                                        "
    .text "Anyways, once the main bits were proto- "
    .text "typed the conversion to the actual C64  "
    .text "was pretty mechanical/trivial. I even   "
    .text "use the same timings as the EoD impl,   "
    .text "because why not :) . And I think the    "

    .text "effect turned out pretty nice. Would've "
    .text "been nice to get a couple more color and"
    .text "motion variations, and have it fade in  "
    .text "and out instead of just popping color   "
    .text "changes, but that was cut in order to   "
    .text "make the 4k limit, which I think was a  "
    .text "fair choice. It still has the impact I  "
    .text "was after I think, so I'm satisfied :) ."
    .text "                                        "
    .text "Ok, you're probably a bit sick of read- "
    .text "ing about this effect; time to move on.."
    .text "                                        "
    .text "LAYERS                                  "
    .text "======                                  "
    .text "                                        "
    .text "So, when thinking of effects I could add"
    .text "to the demo, I watched a bunch of recent"
    .text "demos to see if there were any kinds of "
    .text "effects I hadn't seen before. And while "
    .text "watching 'The Concert' by Performers, I "
    .text "found one such effect - a screen that   "
    .text "looks like it's two char layers, where  "
    .text "one is masked by a third showing the    "
    .text "other one underneath. So, I set out to  "
    .text "understand how it works and do a simpli-"

    .text "fied version for this prod!             "
    .text "The main gist is that there's only two  "
    .text "layers. The back layer is sprites and is"
    .text "set up to display 8x8 blocks and look   "
    .text "like another char layer. A key thing to "
    .text "note here is that colors are only unique"
    .text "per 8x8 row of blocks, not column. Since"
    .text "sprites only have 3 unique colors, this "
    .text "can be (relatively-)easily achieved by  "
    .text "firing an interrupt mid-way in each text"
    .text "row to set the last two colors (the font"
    .text "is 5 pixels high) for the row. In the   "
    .text "original effect, 7 sprites are used, and"
    .text "display what appears to be pre-drawn    "
    .text "data that's loaded progressively. For   "
    .text "added fun, the 8th sprite is even shown "
    .text "with cute cartoon graphics throughout   "
    .text "the screen.                             "
    .text "                                        "
    .text "To simplify the back layer in my impl,  "
    .text "I chose to limit the font to 3x5 chars  "
    .text "and use all 8 sprites to display the    "
    .text "text. Instead of precalculating all of  "
    .text "the data for the layer, the characters  "
    .text "are unpacked progressively from a pre-  "

    .text "processed string of characters (trans-  "
    .text "lated into direct font data offsets) and"
    .text "a block of font data. The unpacking is  "
    .text "pretty simple due to the fact that un-  "
    .text "expanded sprites are already 24 pixels  "
    .text "wide, so we only have to set a few bytes"
    .text "per line based on bits of the font. The "
    .text "sprites are y-expanded to cover the full"
    .text "5x8 pixels height of the font, plus a   "
    .text "couple more pixels that are left blank. "
    .text "                                        "
    .text "The front layer in the original effect  "
    .text "is the more interesting one. Here, the  "
    .text "color memory is used to display a pre-  "
    .text "pixelled image in 8x8 scale. The screen "
    .text "is in hires mode, and some precalculated"
    .text "charsets are used to mask out the image."
    .text "Different charsets are used as different"
    .text "frames of an animation, that lasts as   "
    .text "long as it takes for the image to scroll"
    .text "a single 8-pixel high block. At that    "
    .text "point the cycle will repeat and the     "
    .text "screen will shift downwards while the   "
    .text "data is shifted upwards. Special care   "
    .text "must be taken to produce the charset(s) "

    .text "used so that they don't appear to scroll"
    .text "along with the color mem layer. All tog-"
    .text "ether this creates a really cool mix of "
    .text "motion and an illusion that there's more"
    .text "than just two layers.                   "
    .text "                                        "
    .text "In my original implementation, several  "
    .text "different patterns are used two screen  "
    .text "mem areas are used to progressively load"
    .text "char indices for the next pattern while "
    .text "the current one is displayed. The pat-  "
    .text "terns are all generative to save space. "
    .text "I even included a special text renderer "
    .text "that drew to the color layer in addition"
    .text "to the sprite layer in order to have    "
    .text "both layers' content match in the beg-  "
    .text "inning of the effect, and then suddenly "
    .text "seperate in scrolling, revealing the    "
    .text "mask - something that is very nicely-   "
    .text "presented in the original. However, with"
    .text "all of the different pattern generators,"
    .text "and all of the various scheduling code  "
    .text "required to even get all of this to dis-"
    .text "play properly, the effect was simply too"
    .text "big, so I ended up cutting all but one  "

    .text "pattern, dropping the second text ren-  "
    .text "derer, and simplifying the toggles/color"
    .text "tables for getting some extra colors in "
    .text "the sprite layer (oh yeah, that happens "
    .text "too by alternating which colors are dis-"
    .text "played each frame, fun stuff!). It was a"
    .text "bit sad to cut all of this for this ver-"
    .text "sion of the effect, but I think it was a"
    .text "good choice, especially as this was in  "
    .text "the last few days before the party. And,"
    .text "on the bright side, it looks like I've  "
    .text "already found another group's demo to   "
    .text "put the original implementation into, so"
    .text "at least the full effect will end up in "
    .text "SOME demo in the near future. Stay tuned"
    .text "for that!                               "
    .text "                                        "
    .text "SINE BOXES                              "
    .text "==========                              "
    .text "                                        "
    .text "So, this effect is actually a bit of an "
    .text "anachronism, as I actually wrote the    "
    .text "basis of it when I was 17. Since this   "
    .text "was going to be my first C64 release I  "
    .text "really wanted to include some code from "

    .text "that time, and this effect was pretty   "
    .text "small, so it was going to be a good fit."
    .text "                                        "
    .text "The original implementation was made for"
    .text "NTSC machines (I'm from Idaho, after    "
    .text "all), and somehow I never thought to    "
    .text "double-buffer the effect or have my     "
    .text "frame interrupt occur at the bottom of  "
    .text "the screen rather than the top. This    "
    .text "meant that update time for the sprite   "
    .text "data was fairly limited, so I interlaced"
    .text "the data update and constrained the     "
    .text "movement a bit. I also had a blank back-"
    .text "ground, which wasn't quite good enough. "
    .text "                                        "
    .text "In taking another look at the effect, I "
    .text "wasn't really happy with these solutions"
    .text "and figured I could get it faster, hope-"
    .text "fully to the point of being able to have"
    .text "another char-based effect behind it. As "
    .text "it turns out I was right - I got it so  "
    .text "much faster, in fact, that I was able to"
    .text "have the background char effect update  "
    .text "the entire char matrix each frame, AND I"
    .text "got to remove the interlacing, and all  "

    .text "without double-buffering, which helps to"
    .text "keep the effect small.                  "
    .text "                                        "
    .text "With that I threw in the wavy char bg   "
    .text "effect. Originally we were going to dis-"
    .text "play the image in multicolor mode, but  "
    .text "ended up going for hires and keeping    "
    .text "black in color memory for the entire    "
    .text "demo, which cuts down on update code and"
    .text "extra color ramp tables, so that was a  "
    .text "pretty good choice. Limits the graphics "
    .text "possibilities for the effect a bit, but "
    .text "also was a good excuse to include our   "
    .text "binary logo which I convinced wobble to "
    .text "draw, so I'm pretty happy about that :D."
    .text "Also with some simple table magic and a "
    .text "bit of brain twisting I got the char    "
    .text "updates to happen in 3 instructions per "
    .text "char - lda, adc, sta - which I'm also   "
    .text "pretty happy with. Granted that adc     "
    .text "should really be clc, adc for correct-  "
    .text "ness, but if it looks fine and nobody   "
    .text "notices, then it's already correct, no? "
    .text "..also that's the only way this is fast "
    .text "enough, so.. :D                         "

    .text "INTRO TEXT                              "
    .text "==========                              "
    .text "                                        "
    .text "This is the simplest routine of the demo"
    .text "by far, but is presented in a nice way  "
    .text "that sets up the attitude of the prod   "
    .text "nicely I think.                         "
    .text "                                        "
    .text "The effect itself just sets the whole   "
    .text "screen to black and only writes text    "
    .text "into the first char row of screen mem.  "
    .text "FLD is used to delay the display of this"
    .text "char row, and background+screen color   "
    .text "changes are used to make the text visi- "
    .text "ble. Even here there are some size comp-"
    .text "romizes - the text is chosen to be mini-"
    .text "mal and small, a single color ramp is   "
    .text "used, and the motion only uses one sinus"
    .text "rather than a couple mixed, which would "
    .text "have looked a bit better, but in the end"
    .text "this still gets the point across nicely."
    .text "And, it doesn't take much CPU time (ex- "
    .text "cept for the FLD waits), so we're able  "
    .text "to load the plasma and sine box effects "
    .text "both underneath, which is nice.         "

    .text "PACKING                                 "
    .text "=======                                 "
    .text "                                        "
    .text "Not really an effect ofc, but something "
    .text "to talk about here nonetheless :D . This"
    .text "project started while I was working on  "
    .text "our 64k pc packer (which is still a WIP)"
    .text "so I had a lot of compression stuff on  "
    .text "my brain when embarking on the project. "
    .text "There are already quite good compress-  "
    .text "ors available (for example exomizer)    "
    .text "that are extremely well-balanced in     "
    .text "terms of ratio, unpacking speed, and    "
    .text "unpacker size. But in the end I just    "
    .text "wanted to do my own for fun, and indeed "
    .text "it was quite fun :) .                   "
    .text "                                        "
    .text "My packer is called Admiral P4kbar, a   "
    .text "pun that contains both '4k' and 'packer'"
    .text "in some form or another. It's an LZ(SS)-"
    .text "based packer with optimal parsing and   "
    .text "some custom stuff for backreference dis-"
    .text "tance and length encoding, which really "
    .text "is the special sauce here. I had origin-"
    .text "ally tried to make a packer where only  "

    .text "gamma codes were used to see if I could "
    .text "trade a bit of ratio at this size range "
    .text "for a smaller unpacker, however, this   "
    .text "turned out to perform significantly     "
    .text "worse than exomizer, so I dug in a bit  "
    .text "to see what it was doing. I ended up    "
    .text "taking some ideas there (the table-based"
    .text "number encoding especially) and wound up"
    .text "with something slightly simpler, but    "
    .text "compresses slightly worse and with a    "
    .text "slightly larger decompressor, but it was"
    .text "good enough to please my ego that I had "
    .text "done my own and I had to put it down to "
    .text "focus on the effects in time, hehe. The "
    .text "last test I did against exomizer was    "
    .text "when we only had the music player, the  "
    .text "zoomplasma, and our current sine table  "
    .text "generation method, at which point my    "
    .text "packer got us down to about 2.5kb. Exo- "
    .text "mizer beat this by around 60 bytes all  "
    .text "together. If we extrapolate that to the "
    .text "final 4kb size, exomizer probably would "
    .text "have saved around 100 bytes, which most "
    .text "likely would have translated to slightly"
    .text "better sync/color ramps, and that's     "

    .text "about it, so I'm pretty satisfied. But  "
    .text "if (or when, rather) we do another C64  "
    .text "4k, this is something I'd like to re-   "
    .text "visit to see if I can beat exomizer.    "
    .text "Lots of experimentation/tweaking to do  "
    .text "on that front, which should be quite    "
    .text "fun.                                    "
    .text "                                        "
    .text "FINAL WORDS (from me at least)          "
    .text "==============================          "
    .text "                                        "
    .text "So altogether I think this was a REALLY "
    .text "fun project to work on, and it sure as  "
    .text "hell won't be our last C64 prod, much   "
    .text "less our last C64 4k. There are a lot of"
    .text "other cool C64 4k's around these days,  "
    .text "but I think there's still some room for "
    .text "more of them that feel like tiny demos  "
    .text "and push the limits a bit further - and "
    .text "it seems there are some whispers in the "
    .text "wind of some other folks that feel the  "
    .text "same way.. let's say this - logicoma    "
    .text "won't be the only previously-non-C64    "
    .text "group making C64 4k's this year! That's "
    .text "quite exciting I think :) .             "

    .text "Anyways, by the time you read this, the "
    .text "full source to the intro can be found   "
    .text "here:                                   "
    .text "https://github.com/yupferris/makeshift  "
    .text "                                        "
    .text "Cheers to h0ff and wobble and 4mat for  "
    .text "working on this prod, it meant a lot to "
    .text "me to finally be able to put something  "
    .text "out on C64, and something that turned   "
    .text "out so rad and solid (again, if I do say"
    .text "so myself, hehe) and even has our logi- "
    .text "coma style - really proud of that. And  "
    .text "I'm also quite proud of being able to   "
    .text "release this at Solskogen, so close to  "
    .text "home. And finally cheers to you for     "
    .text "reading through all this - the notes are"
    .text "some of my favorite parts of C64 prods, "
    .text "and I hope someday someone reading this "
    .text "will get the same feeling of wonder and "
    .text "excitement that I did when first        "
    .text "discovering the notefiles on C64 disks  "
    .text "describing technical details of the     "
    .text "effects, with cool presentation.        "
    .text "                                        "
    .text "Ferris out.                             "

    .text "NOTES ON SOUND - HOFFMAN                "
    .text "                                        "
    .text "Hello reader, Hoffman here. Let us kick "
    .text "this text off by talking about the tune "
    .text "you're listening too in this note. It's "
    .text "called Source Flux and I wrote it back  "
    .text "in 1997 (yes 20 years ago!). That year I"
    .text "wrote 4 tunes using DMC and hadn't      "
    .text "written one since, until now.           "
    .text "                                        "
    .text "Ferris started prototyping the plasma   "
    .text "effect and once he got it working on the"
    .text "C64 we thought we'd have a crack at     "
    .text "making in into a 4k it. He'd already    "
    .text "made some test tracks in CheeseCutter   "
    .text "but even with just a basic single       "
    .text "pattern it was packing down to over well"
    .text "over 2kb, simply wasn't going to cut it."
    .text "I then remembered having a chat with Rez"
    .text "at Revision last year about how he      "
    .text "worked with 4mat on their 4k intro      "
    .text "(http://bit.ly/2uZlCkG). Matt graciously"
    .text "gave us the source and example tune for "
    .text "his 1K Play system and that is exactly  "
    .text "what it is, source code, no editor, no  "

    .text "GUI, just you and a text editor. Ferris "
    .text "integrated the example track into the   "
    .text "framework and it sized up really well.  "
    .text "                                        "
    .text "The system itself is pretty well        "
    .text "featured, 256 bytes of song data, 256   "
    .text "bytes of pattern data, 15 patterns and  "
    .text "16 instruments. It also has pulse width "
    .text "and pitch mod, although only increment, "
    .text "and a bi-directional filter mod. It also"
    .text "has a funk tempo option, which we ended "
    .text "up using to get the right pace in our   "
    .text "intro.                                  "
    .text "                                        "
    .text "It has been a tricky mission getting the"
    .text "music working for this. One of the big  "
    .text "hurdles is the instrument tables. There "
    .text "are two separate tables, one for pitch  "
    .text "and one for waveform. The instrument has"
    .text "a single offset value for both tables,  "
    .text "so if you accidently add an extra byte  "
    .text "to one of the tables, it throws the     "
    .text "other out of alignment. It also means if"
    .text "you delete an unused instrument, all the"
    .text "subsequent instruments need their values"

    .text "adjusting. By the time I worked out a   "
    .text "better solution using constants which   "
    .text "auto calculate the table offsets, most  "
    .text "of the tune had been written.           "
    .text "                                        "
    .text "The pattern data is also pretty tricky. "
    .text "It essentially has 4 commands, set      "
    .text "instrument, set note length, play note  "
    .text "and end pattern. It does mean that you  "
    .text "save a lot of space when compared to a  "
    .text "normal tracker as all the dead space    "
    .text "between notes isn't there, however it   "
    .text "does make it very hard to read.         "
    .text "                                        "
    .text "The song data was a lot simpler using a "
    .text "single byte split into transpose value /"
    .text "pattern number. The transpose is really "
    .text "handy as you can re-use a lot of the    "
    .text "pattern data by shifting up a few       "
    .text "semitones. As the pitch commands in the "
    .text "instrument can have hard coded values,  "
    .text "you can easily transpose a mixed        "
    .text "percussion and melody section and keep  "
    .text "everything sounding correct. It also has"
    .text "a repeat next pattern function which can"

    .text "save quite a few bytes.                 "
    .text "                                        "
    .text "There had been many times during        "
    .text "development where I would change a      "
    .text "single hex value which would result in a"
    .text "full-on crash or just weird stuff       "
    .text "happening to notes or instruments, but  "
    .text "they help with understanding how things "
    .text "work, or don't work.                    "
    .text "                                        "
    .text "Once I had basic melody and drums       "
    .text "running that we were happy with, we did "
    .text "our usual thing of working separately,  "
    .text "meaning I was working on making the     "
    .text "soundtrack without considering how it   "
    .text "would fit together with the intro. I'd  "
    .text "managed to get around 3 minutes penned  "
    .text "out in an arrangement and all still     "
    .text "within the limits of the player. Once   "
    .text "Ferris got all the intro parts          "
    .text "integrated and saw how large the whole  "
    .text "thing was, we decided to reduce it down "
    .text "to 2 minutes. This was pretty easy to   "
    .text "trim down and slap a proper ending on.  "
    .text "It also gave us the opportunity to pick "

    .text "out our favourite parts to run with.    "
    .text "                                        "
    .text "I primarily wrote the tune for the 8580 "
    .text "chip, mainly because I only own a C64C  "
    .text "but, I have made a version for the 6581 "
    .text "chip which addresses the filter         "
    .text "difference. It doesn't sound the same   "
    .text "due to the different waveform           "
    .text "characteristic between the chips, but   "
    .text "it's at least a little closer.          "
    .text "                                        "
    .text "So, there you have it, first SID in 20  "
    .text "years and not an easy one to put        "
    .text "together either. It's been very         "
    .text "challenging but also a really fun       "
    .text "project to work. Massive thanks again to"
    .text "4Mat for lending us his hard work, we   "
    .text "really appreciate pal.                  "
    .text "                                        "
    .text "Shouts to the UK Demoscene slack crew,  "
    .text "Logicoma fam, Focus Design fam and all  "
    .text "the people keeping the scene alive.     "
    .text "                                        "
    .text "                                        "
    .text "Hoff out.                               "
text_end:
