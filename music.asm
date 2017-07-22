
// "1K PLAY" music driver for reasonably small c64 songs.   Used in Razor 1911's "The Best Intro ever" at Revision 2016.
// code by 4mat / ate bit + orb
//  slightly modified by ferris / logicoma to be compatible [enough] with KickAssembler etc
// music by h0ffman / logicoma

// There are some questionable design decisions in the code :), but if you want 1kb-ish songs with filters and some fx it'll do that.

// Main work done in 2003
// Some small fixes in 2016 (actually the first time I've used it in a prod)

// Requires : ~~DASM Assembler~~ KickAssembler :)

// Useage:
// -------
// DEBUGMODE = 0 - just build driver for use in production.
// Standard calls:  Init Player - JSR ADDRESS , Call update each frame - JSR ADDRESS+$03

// DEBUGMODE = 1 - build with front-end. (for writing tracks/testing)

// Press SPACE to ffwd through song.
// Press FIRE (JOY #2) to reset max rastertime counter. (you'll need to do that when using ffwd as well)

// In this mode the song and pattern position offsets are AUTOMATICALLY CALCULATED, when building just the driver you need to insert these values into the tables at the end.
// You can find the calculated tables in memory at $0340 - song positions (3 bytes) , $0380 - pattern positions (however many patterns you have)

.const DEBUGMODE = 0

// Relocation/Footprint
// --------------------
// On top of the driver size player uses some ram during run-time: 256 bytes for the generated frequency table and $35 bytes of space for variables.
// You can relocate the player, freq. table and variables here:

.const ADDRESS = $0a00   // Player start address
.const freqtablo = $0200 // Frequency table start address (256 bytes)
.const songpos = $c0     // Variables start address ($35 bytes)

// Obviously putting the player variables outside zero page will increase the size of the player at run-time.

// Saving more ram
// ---------------
// There are a couple of player functions that can be disabled to save more ram if you don't use them:

.const FUNKTEMPO = 1  // If using one constant tempo (rather than 'funk tempo' on alternate frames) set this to 0.
.const SONGREPEAT = 1 // If you aren't using the Ex command in your song data (repeat next pattern) set this to 0.

// Writing songs
// --------------
// Scroll to bottom of source for instructions. (search for Music Data)

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

.const freqtabhi = freqtablo+$80
.const pattpos = songpos+$03
.const transpose = songpos+$06
.const note = songpos+$09
.const instnum = songpos+$0c
.const delay = songpos+$0f
.const repeaty = songpos+$12
.const yahoo = songpos+$15
.const instfreq = songpos+$18
.const insthigh = songpos+$1b
.const notelen = songpos+$1e
.const patwait = songpos+$21
.const resetwav = songpos+$24
.const patrepeat = songpos+$27
.const transtemp = songpos+$2a
.const filtset = songpos+$2d
.const ticks = songpos+$2e
.const tempocnt = songpos+$2f
.const temp = songpos+$34 
.const temp2 = songpos+$33 
.const temp3 = songpos+$32 
.const temp4 = songpos+$31 
.const temp5 = songpos+$30 

// Debug display

    .if(DEBUGMODE == 1) {
    
    .pc = $0801
    .byte   $0b,$08,$ef,$00,$9e,$32,$30,$36,$32,0,0,0,0

    .pc = $080e

start:
    
    jsr $e544
    sei
    
    jsr init
    
    ldy #$00
    ldx #$00
showtext:
    lda screengfx,x
    sta $0400,x
    lda screencol,y
    sta $d800,x
    sta $d900,x
    iny
    cpy #$50
    bne notapage
    ldy #$00
notapage:
    inx
    bne showtext

    clc
    lda #>ADDRESS
    jsr gethex
    lda hexdisp
    sta $04fb
    lda hexdisp+$01
    sta $04fc

    lda #<ADDRESS
    jsr gethex
    lda hexdisp
    sta $04fd
    lda hexdisp+$01
    sta $04fe
    
    lda #>codeend
    jsr gethex
    lda hexdisp
    sta $0500
    lda hexdisp+$01
    sta $0501

    lda #<codeend
    jsr gethex
    lda hexdisp
    sta $0502
    lda hexdisp+$01
    sta $0503
    
    
loop:


    lda $d012
    cmp #$40
    bne loop
    sta rasterstart

skiprast:

    inc $d020
    jsr playframe
    lda $d012
    sta rasternow 
    dec $d020

    clc
    lda rasternow 
    sbc rasterstart 
    sta rastercurrent 
    cmp rastermax 
    bcc notnewraster
    sta rastermax 
    
notnewraster:

    jsr displaydata 
    
    lda $dc01
    cmp #$ef
    bne noskiprast

    jmp skiprast
    
noskiprast:
    
    lda $dc00
    cmp #$6f
    bne loop
    
    lda #$00
    sta rastermax 
    
    jmp loop

displaydata:

    ldx #$00
display:
    lda dispreglo,x
    sta readreg+$01
    lda dispreghi,x
    sta readreg+$02
    ldy dispoffset,x
readreg:
    lda $4000,y
    jsr gethex
    lda displo,x
    sta print1+$01
    clc
    adc #$01
    sta print2+$01
    lda hexdisp
print1:
    sta $0400
    lda hexdisp+$01
print2:
    sta $0428
    inx
    cpx #$0f
    bne display
    
    rts

rasterstart:
    .byte $00
rastermax:
    .byte $00
rasternow:
    .byte $00
rastercurrent:
    .byte $00
    
dispreglo:
    .byte <songpos,<songpos,<songpos 
    .byte <pattpos,<pattpos,<pattpos 
    .byte <note,<note,<note 
    .byte <instnum,<instnum,<instnum 
    .byte <ticks 
    .byte <rastercurrent 
    .byte <rastermax 

dispreghi:

    .byte >songpos,>songpos,>songpos 
    .byte >pattpos,>pattpos,>pattpos 
    .byte >note,>note,>note 
    .byte >instnum,>instnum,>instnum 
    .byte >ticks 
    .byte >rastercurrent 
    .byte >rastermax 
    
dispoffset:
    .byte $00,$01,$02
    .byte $00,$01,$02
    .byte $00,$01,$02
    .byte $00,$01,$02
    .byte $00
    .byte $00
    .byte $00
    
displo:
    .byte $18,$1c,$20
    .byte $40,$44,$48
    .byte $68,$6c,$70
    .byte $90,$94,$98
    .byte $b8
    .byte $e0,$e3

screengfx:
    .text "song pos                                "
    .text "pattern pos                             "
    .text "note                                    "
    .text "instrument                              "
    .text "frame tick                              "
    .text "rastertime/max time       /      1k*play"
    .text "song size $    -"

screencol:
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$0d,$0d,$01,$01,$0d,$0d,$01,$01,$0d,$0d,$01,$01,$07,$03,$0d,$01
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$07,$07,$01,$07,$07,$07,$01,$01,$07,$07,$01,$01,$07,$03,$0d,$01
    
gethex: 
    pha

    and #$0f
    tay
    lda hexchars,y
    sta hexdisp+$01

    pla
    lsr
    lsr
    lsr
    lsr
    tay
    lda hexchars,y
    sta hexdisp
    rts

hexdisp:
    .byte $00,$00
    
hexchars:
    .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$01,$02,$03,$04,$05,$06

autofind:

    ldy #$01
    ldx #$00
songloop:
    lda songtab,x
    inx
    cmp #$ff
    bne songloop
    txa
    sta songstart,y
    sta $0340,y
    iny
    cpy #$03
    beq dothepat
    jmp songloop
dothepat:
    ldy #$01
    ldx #$00
pattloop: 
    lda patttab,x
    cmp #$7f
    bne notpatt
    inx
    txa 
    sta pattstart,y
    sta $0380,y
    iny
    cpy #$10
    beq dotherest
    jmp pattloop
notpatt:
    inx
    bne pattloop 
dotherest:
    rts

    }

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Driver

    .pc = ADDRESS
    jmp init
    jmp playframe

init:
    
    ldx #$00
    stx freqtablo
freqloop:
    lda #$01 
    pha
    txa
    tay
freqset:
    pla
    sta freqtablo+$01,y 
    asl
    pha
    lda freqsource,x
    sta freqtabhi+$01,y 
    rol
    sta freqsource,x
    bcc notinc
    pla
    adc #$00
    pha
    
notinc:
    clc
    tya
    adc #$0c
    tay
    bpl freqset
    pla
    inx
    cpx #$0c 
    bne freqloop 
    
    .if(DEBUGMODE == 1) {
    
    // TODO: Why doesn't kickassembler find this symbol?
    //jsr autofind
    
    }
    
    ldx #$34
    lda #$00
cleardata:
    sta songpos,x
    dex
    bpl cleardata

    ldx #$02
resetgo:
    lda songstart,x
    sta songpos,x
    dex
    bpl resetgo
    rts       

playframe:

    ldx #$00
    stx temp2

noteloop:
    stx temp
    lda ticks
    beq dothis
    jmp update

dothis:
    dec delay,x
    bmi newnote
    jmp update

newnote:
    lda transtemp,x
    sta transpose,x

    ldy pattpos,x
    lda patttab,y

nonewpat:
    cmp #$ef
    bcc justnote
instrument:
    and #$0f
    sta instnum,x
fubme:
    inc pattpos,x
    jmp newnote

justnote:
    bpl regularnote
    and #$0f
    sta notelen,x 
    bpl fubme

regularnote:
    pha

    ldy instnum,x
    lda notelen,x
    sta delay,x
    inc pattpos,x
    pla 
    cmp #$00
    beq update
    sta note,x

doit:

    lda instoff,y
    sta yahoo,x
    

    lda resetwav,x
    cmp instnum,x
    bne havetoreset

    lda instdec,y
    and #$f0
    bne nowavreset

havetoreset:

    lda instpul,y
    and #$0f
    sta insthigh,x
    lda #$00
    sta instfreq,x

nowavreset:

    lda instfilt,y
    beq nofilt
    sta filtset
    lda filtres,y
    sta $d417
    lda volume,y
    sta $d418
nofilt:
    tya
    sta resetwav,x


    ldx temp2

    lda #$09
    sta $d404,x
    lda instadc,y
    sta $d405,x
    lda instsus,y
    sta $d406,x


update:

    ldx temp

    lda instnum,x
    sta temp3



    ldy yahoo,x
    lda instnote,y
    ldy temp2
    cmp #$00
    beq playusual
    cmp #$f0
    bcc playthis
    and #$0f
    bpl playusual
playthis:
    sta temp5
    jmp playcont

playusual:
    clc
    adc note,x
    adc transpose,x

playnowt:
    tax
    lda freqtablo,x
    sta temp5
    lda freqtabhi,x
    sta temp4

    ldx temp
    ldy temp3
    lda instdec,y
    and #$01
    beq nowave
    lda temp5
    adc instfreq,x
    sta temp5

nowave:

    
    
    ldy temp2
    


playcont:

    lda temp5
    sta $d401,y
    lda temp4
    sta $d400,y

    lda instfreq,x
    sta $d402,y
    lda insthigh,x
    sta $d403,y

    lda yahoo,x
    tax
    lda insttab,x
    sta $d404,y

nogate:
    
    ldx temp


    ldy temp3
    lda instfreq,x
    adc instadd,y 
    sta instfreq,x
    lda instfreq,x
    cmp instadd,y
    bcs noise
    inc insthigh,x
    


noise:


    inc yahoo,x
    ldy yahoo,x
    
    lda insttab,y
    cmp #$ef
    bcc noinstres
    and #$0f
    sta temp
    lda yahoo,x
    sbc temp
    sta yahoo,x
noinstres:
    clc
    lda temp2
    adc #$07
    sta temp2


    ldy pattpos,x
    lda patttab,y
    cmp #$7f
    bne notfin
    

getpat:


    ldy songpos,x
    
    lda songtab,y
    cmp #$ff
    bne nopatreset

    lda songstart,x
    sta songpos,x
    jmp getpat

nopatreset:

    .if(SONGREPEAT == 1) {
    cmp #$df
    bcc nopatloop
    
yesrepeat:
    and #$0f
    sta patwait,x
    inc songpos,x
    jmp getpat
    }
    
nopatloop:
    sta temp
    and #$0f
    tay 
    lda pattstart,y
    sta pattpos,x
    lda temp
    and #$f0
    lsr
    lsr
    lsr
    lsr
    sta transtemp,x 


    dec patwait,x
    bpl notfin
    inc songpos,x
    lda #$00
    sta patwait,x
notfin:


    inx
    cpx #$03

    beq updatend
    jmp noteloop 
updatend:

    lda ticks
    bne noreset

    .if(FUNKTEMPO == 1) {
    inc tempocnt
    lda tempocnt
    and #$01
    sta tempocnt
    tax
    lda tempo,x
    
    } else {
    
    lda tempo

    }

    sta ticks 
    ldy temp3
    lda filtset
    adc instfila,y
    sta filtset
    sta $d416
noreset:
    dec ticks
    rts

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Music Data
// ----------

// A song can have:

// 256 bytes total of song positon data.
// 256 bytes total of pattern data.
// 15 patterns (pattern 0 must stay blank for init)
// 16 instruments
// Single or dual 'funk' tempo. (flipping between these values each beat if you have FUNKTEMPO enabled. Can also be used for intermediate tempos (eg: $06,$07) 

// Tempo settings. (amount of ticks per beat like other trackers)

tempo:
    .byte $06,$05

// Song table offset positions for each channel.  If in DEBUG mode look at memory $0340-$0342 for the offset positions you need to put here when building song in non debug mode.

songstart:

    .byte $00,$1f,$50

// Song data table.  Song format is:

// $XY = Transpose ($0-$D) / Pattern number ($1-$F) - eg: $01 = No transpose & play pattern 1 , $75 = +7 Transpose and play pattern 5.
// $Ex = Repeat following pattern x times           - eg: $E7 = Repeat next pattern 7 times.
// $FF = End song channel. (must have 3 channels setup in a song)

songtab:  
    // channel 1
    .byte $e9,$51,$54,$e9,$51,$54                                   // arp intro
    .byte $e2,$01,$31,$31,$e4,$01,$54,$e2,$71,$31,$71,$e2,$c1,$c9   // arp intro
    .byte $ef,$02   // drums
    .byte $ef,$07   // drums and arp
    .byte $ef,$02   // drums

    .byte $ec,$c1
    .byte $0b
    .byte $0b
    .byte $ff

    // channel 2
    .byte $e9,$51,$54,$e9,$51,$54   // arp intro
    .byte $e9,$51,$54,$e7,$51,$09   // arp intro
    .byte $ef,$05   // new arp
    .byte $e7,$06   // new chord stab
    .byte $e2,$01,$31,$31,$e4,$01,$54,$e2,$71,$31,$71,$e4,$51,$54   // arp mid
    .byte $e2,$01,$31,$31,$e4,$01,$54,$e2,$71,$31,$71,$e4,$51,$54   // arp mid

    .byte $ec,$51
    .byte $0b
    .byte $0b
    .byte $ff 
    
    // channel 3
    .byte $08
    .byte $08
    .byte $e3,$03
    .byte $08
    .byte $08
    
    .byte $0a
    
    .byte $0b
    .byte $0b
    .byte $ff

        
// Pattern table offset positions. 
// As with song data if in DEBUG mode look at memory $0380-$038F for offset positions you need to put here.  When in debug mode leave this table 16 bytes long
// so the auto-find code doesn't overwrite your other data.
         
pattstart:
    .byte $00,$02,$0b,$23,$3e,$43,$56,$67,$7e,$89,$8d,$96,$9a,$cb,$d0,$00

// Pattern data table.  Pattern format is:

// $00     - Blank note.  Will leave previous note playing if one is enabled.
// $01-$60 - Play note.  See table below.
// $8x     - Set duration for all following notes. (+1 for length) - eg: $87 = All following notes will play for 8 beats.
// $Fx     - Set instrument for all following notes.               - eg: $F5 = All following notes will use instrument 5.
// $7F     - End pattern.

// Always leave pattern 0 blank, the default below is smallest you need.

// Note table guide
         //      *        *       *             *        *          
         // c   c#   d   d#   e   f   f#   g   g#   a   a#   b
  
         // 1   2    3   4    5   6   7    8   9    a   b    c
         // d   e    f   10   11  12  13   14  15   16  17   18
         // 19  1a   1b  1c   1d  1e  1f   20  21   22  23   24
         // 25  26   27  28   29  2a  2b   2c  2d   2e  2f   30
         // 31  32   33  34   35  36  37   38  39   3a  3b   3c
         // 3d  3e   3f  40   41  42  43   44  45   46  47   48
         // 49  4a   4b  4c   4d  4e  4f   50  51   52  53   54
         // 55  56   57  58   59  5a  5b   5c  5d   5e  5f   60
         
          
patttab: 

        // do what 4mat says!  believe me!
    .byte $00,$7f // ALWAYS LEAVE THIS PATTERN (0) AS IS

        // BASIC ARP                01
    .byte $f0,$80,$25,$31,$3d,$25,$31,$3d,$7f

        // DRUMS PART1              02
    .byte $80,$f1,$01,$00,$f2,$55,$40,$55,$50,$f1,$31,$00,$f2,$55,$30,$50,$60,$f4,$31,$00,$f2,$55,$29,$7f       

        // BASS DROP                03
    .byte $f3,$8f,$1a,$8b,$1c,$81,$1c,$81,$1e,$8f,$00,$00
    .byte         $1a,$8b,$1c,$81,$19,$81,$17,$8f,$00,$87,$00,$83,$17,$19,$7f       
    
        // BASIC ARP FILLER         04
    .byte $25,$31,$3d,$25,$7f   

        // UPLIFT ARP               05
    .byte $80,$f6,$36,$39,$2a,$36,$00,$39,$42,$2a,$39,$44,$36,$47,$39,$49,$42,$4e,$7f
            //$36,$39,$2a,$36,$00,$39,$42,$2a,$3b,$47,$36,$49,$39,$47,$42,$4e,$7f  
    
        // OK I FINALLY DID AN PROPER ARP   --> 06
    .byte $f9,$83,$36,$36,$82,$36,$36,$fc,$36,$36,$f9,$36,$36,$fd,$36,$36,$7f
    
        // DRUM ARP                 07
    .byte     $f1,$36,$fb,$39,$2a,$36,$00,$39,$f1,$42,$fb,$2a,$39,$44,$36,$47,$f4,$39,$fb,$49,$42,$4e,$7f
    //.byte     $f1,$36,$fb,$39,$2a,$36,$00,$39,$f1,$42,$fb,$2a,$3b,$47,$f1,$36,$fb,$49,$f4,$39,$fb,$47,$f1,$42,$fb,$4e,$7f  

        // BASS INTRO               08
    .byte $f5,$8f,$1a,$1c,$1e,$00,$1a,$1c,$17,$00,$7f       
    
        // RIZER                    09
    .byte $fa,$8f,$01,$7f   

        // BASS OUTRO               0A
    .byte $f7,$8d,$12,$8f,$00,$00,$00,$00,$7f

    // killer                       0B
    .byte $8f,$f8,$01,$7f
    
        // Hoffmans attempt at making instrument juggling a bit easier
.const i00 = ins00-insttab
.const i01 = ins01-insttab
.const i02 = ins02-insttab
.const i03 = ins03-insttab
.const i04 = ins04-insttab
.const i05 = ins05-insttab

.const i07 = ins07-insttab
.const i08 = ins08-insttab
.const i09 = ins09-insttab
.const i10 = ins10-insttab
.const i11 = ins11-insttab
    
// Instrument settings

// instoff   : Offset for instrument into instrument tables (below)
// instadc   : $XY : Attack/Decay 
// instdec   : $XY : X = 1 don't reset pulsewidth modulation subsequent notes using this instrument (Rob Hubbard-style) / Y = 1 means add frequency slide enabled. (for waveforms that aren't pulse)
// instsus   : $XY : Sustain/Release 
// instpul   : Pulsewidth coarse value ($00-$0f) 
// instadd   : Amount to add to Pulsewidth modulation or pitch frequency if enabled in 'instdec' 
// volume    : $XY : Filter type / Instrument Volume (see $d418 settings in C64 manual) 
// filtres   : $XY : Filter resonance / Channel matrix that can use filter (see $d417 settings in C64 manual)  
// instfilt  : Filter cut-off start position , 00 = no filter enabled on instrument.  Last instrument on a beat that has filter enabled will have control of the filter.
// instfila  : Amount to add to filter cut-off each tempo pass.  
            //   A   K   H   B   S   B   A   O   K   K   R   A   K   K                           
            //   R   I   A   A   N   S   P   T   I   R   I   R   R   R                         
            //   P   K   T   S   R   2   2   1   L   1   Z   3   2   3
                                                                     
            //   F0  F1  F2  F3  F4  F5  F6  F7  F8  F9  FA  FB  FC  FD
                                                                     
instoff:  .byte i00,i01,i02,i03,i04,i03,i05,i03,i02,i09,i07,i08,i10,i11
instadc:  .byte $10,$10,$10,$10,$10,$10,$10,$10,$00,$10,$d0,$00,$10,$10
instdec:  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$10,$01,$00,$10,$10
instsus:  .byte $4f,$f9,$34,$ef,$f8,$9f,$7f,$9f,$00,$9c,$af,$7f,$9c,$9c
instpul:  .byte $06,$09,$00,$04,$08,$00,$04,$00,$00,$01,$00,$04,$01,$01
instadd:  .byte $e0,$00,$00,$20,$00,$2e,$80,$2e,$00,$30,$01,$00,$60,$60
volume:   .byte $0f,$0f,$0f,$1f,$0f,$1f,$00,$1f,$00,$0f,$0f,$00,$0f,$0f
filtres:  .byte $00,$00,$00,$54,$00,$05,$00,$57,$00,$00,$00,$00,$00,$00
instfilt: .byte $00,$00,$00,$90,$00,$04,$00,$50,$00,$00,$00,$00,$00,$00
instfila: .byte $00,$00,$00,$fb,$00,$00,$00,$fe,$00,$00,$00,$00,$00,$00

// Instrument waveform table

// Start instruments with $09 for a solid restart.
// Use normal C64 waveform settings for instruments.
// Instruments must end with $fX loop command, where X is the amount of steps to jump back at the end of the instrument. (eg : $F1 = jump back one step , $F4 = jump back four steps like an arpeggio loop) 

insttab:  
ins00:  .byte $09,$41,$10,$10,$10,$f3                       // plink
ins01:  .byte $09,$81,$40,$40,$40,$40,$10,$f1           // kick
ins02:  .byte $09,$81,$80,$f1                               // hat
ins03:  .byte $09,$41,$40,$f1                               // basic bass
ins04:  .byte $09,$41,$40,$80,$80,$f2                       // snare 

ins05:  .byte $09,$51,$20,$50,$f1                           // hi arp
    
ins07:  .byte $09,$80,$80,$13,$80,$f2                       // rizer
    
ins08:  .byte $09,$81,$23,$50,$f1                       // hi arp hat
    
ins09:  .byte $09,$21,$23,$20,$20,$20,$20,$20,$20,$20,$20,$20,$f9   
ins10:  .byte $09,$21,$23,$20,$20,$20,$20,$20,$20,$20,$20,$20,$f9   
ins11:  .byte $09,$21,$23,$20,$20,$20,$20,$20,$20,$20,$20,$20,$f9   

// Instrument pitch table.

// Length must match waveform table as same offsets are used for both.
// $00     = Play pitched note
// $01-$ef = Play fixed coarse note pitch. (eg : for drums)
// $f0-$ff = Transpose note $fX amount. (eg : for arpeggios - $f3 = minor key, $f7 = 5th)

instnote:
        .byte $ef,$fc,$00,$00,$fc,$00                       // plinky
        .byte $cf,$10,$0e,$0a,$06,$02,$02,$00               // kick
        .byte $00,$00,$ff,$00                               // hat
        .byte $00,$00,$00,$00                               // basic bass
        .byte $00,$0c,$12,$e0,$20,$00                       // snare

        .byte $00,$00,$fc,$00,$00                           // hi arp
        
        .byte $00,$00,$00,$00,$00,$00                       // rizer
        
        .byte $ef,$a0,$fc,$00,$00                       // hi arp hat

        .byte $f0,$f0,$f3,$f3,$f0,$f0,$f7,$f7,$f3,$f3,$fc,$fc,$00
        .byte $f0,$f0,$f3,$f3,$f0,$f0,$fa,$fa,$f3,$f3,$fe,$fe,$00
        .byte $f0,$f0,$f3,$f3,$f0,$f0,$f5,$f5,$f3,$f3,$fa,$fa,$00
        

        
// Default note pitches for frequency generator.

freqsource:
    .byte 12,28,45,62,81,102,123,145,169,195,221,250
    
codeend: