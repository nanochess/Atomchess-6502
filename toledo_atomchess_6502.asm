        ;
        ; Toledo Atomchess 6502 for Atari VCS/2600
        ;
        ; by Óscar Toledo G. (nanochess)
        ;
        ; © Copyright 2017 Óscar Toledo Gutiérrez
        ;
        ; Creation date: Jan/02/2017. Ported from Toledo Atomchess x86.
        ; Revision date: Jan/04/2017. Working board display logic and selection.
        ; Revision date: Jan/05/2017. Avoid player to move non-white pieces or
        ;                             bug when fire bounces. Now using another
        ;                             color for black pieces. Small optimization.
        ; Revision date: Jan/13/2017. Solved bug where it would answer with move
        ;                             after checkmate. Some more comments.
        ; Revision date: Jan/15/2017. Added size optimizations by Peter Ferrie,
        ;                             19 bytes saved. Also I've optimized my
        ;                             graphical/input interface for further 18
        ;                             bytes.
        ; Revision date: Jan/16/2017. Saved 2 bytes more in playfield setup for
        ;                             squares (Ferrie).
        ;

        processor 6502

        ; Features:
        ; * Computer plays legal basic chess movements ;)
        ; * Move with joystick, push button to select, push button to drop.
        ; * Search depth of 2-ply
        ; * Promotion of pawns only to queen.
        ; * No castling
        ; * No en passant.
        ; * 1K self-contained size for Hackaday
        ;
        ; Assemble with dasm from http://dasm-dillon.sourceforge.net/ 
        ; Tested with Stella from http://stella.sourceforge.net/
        ; Tested in real Atari 2600 using Harmony cartridge.
        ; Tested with online emulation from http://8bitworkshop.com/
        ; Demostration video at https://www.youtube.com/watch?v=_Du4krvIl7o

        org $fc00

VSYNC   = $00 ; 0000 00x0   Vertical Sync Set-Clear
VBLANK  = $01 ; xx00 00x0   Vertical Blank Set-Clear
WSYNC   = $02 ; ---- ----   Wait for Horizontal Blank
RSYNC   = $03 ; ---- ----   Reset Horizontal Sync Counter
NUSIZ0  = $04 ; 00xx 0xxx   Number-Size player/missile 0
NUSIZ1  = $05 ; 00xx 0xxx   Number-Size player/missile 1
COLUP0  = $06 ; xxxx xxx0   Color-Luminance Player 0
COLUP1  = $07 ; xxxx xxx0   Color-Luminance Player 1
COLUPF  = $08 ; xxxx xxx0   Color-Luminance Playfield
COLUBK  = $09 ; xxxx xxx0   Color-Luminance Background
CTRLPF  = $0a ; 00xx 0xxx   Control Playfield, Ball, Collisions
REFP0   = $0b ; 0000 x000   Reflection Player 0
REFP1   = $0c ; 0000 x000   Reflection Player 1
PF0     = $0d ; xxxx 0000   Playfield Register Byte 0
PF1     = $0e ; xxxx xxxx   Playfield Register Byte 1
PF2     = $0f ; xxxx xxxx   Playfield Register Byte 2
RESP0   = $10 ; ---- ----   Reset Player 0
RESP1   = $11 ; ---- ----   Reset Player 1
RESM0   = $12 ; ---- ----   Reset Missle 0
RESM1   = $13 ; ---- ----   Reset Missle 1
RESBL   = $14 ; ---- ----   Reset Ball
AUDC0   = $15 ; 0000 xxxx   Audio Control 0
AUDC1   = $16 ; 0000 xxxx   Audio Control 1
AUDF0   = $17 ; 000x xxxx   Audio Frequency 0
AUDF1   = $18 ; 000x xxxx   Audio Frequency 1
AUDV0   = $19 ; 0000 xxxx   Audio Volume 0
AUDV1   = $1a ; 0000 xxxx   Audio Volume 1
GRP0    = $1b ; xxxx xxxx   Graphics Register Player 0
GRP1    = $1c ; xxxx xxxx   Graphics Register Player 1
ENAM0   = $1d ; 0000 00x0   Graphics Enable Missile 0
ENAM1   = $1e ; 0000 00x0   Graphics Enable Missile 1
ENABL   = $1f ; 0000 00x0   Graphics Enable Ball
HMP0    = $20 ; xxxx 0000   Horizontal Motion Player 0
HMP1    = $21 ; xxxx 0000   Horizontal Motion Player 1
HMM0    = $22 ; xxxx 0000   Horizontal Motion Missile 0
HMM1    = $23 ; xxxx 0000   Horizontal Motion Missile 1
HMBL    = $24 ; xxxx 0000   Horizontal Motion Ball
VDELP0  = $25 ; 0000 000x   Vertical Delay Player 0
VDELP1  = $26 ; 0000 000x   Vertical Delay Player 1
VDELBL  = $27 ; 0000 000x   Vertical Delay Ball
RESMP0  = $28 ; 0000 00x0   Reset Missile 0 to Player 0
RESMP1  = $29 ; 0000 00x0   Reset Missile 1 to Player 1
HMOVE   = $2a ; ---- ----   Apply Horizontal Motion
HMCLR   = $2b ; ---- ----   Clear Horizontal Move Registers
CXCLR   = $2c ; ---- ----   Clear Collision Latches

CXM0P   = $00 ; xx00 0000       Read Collision  M0-P1   M0-P0
CXM1P   = $01 ; xx00 0000                       M1-P0   M1-P1
CXP0FB  = $02 ; xx00 0000                       P0-PF   P0-BL
CXP1FB  = $03 ; xx00 0000                       P1-PF   P1-BL
CXM0FB  = $04 ; xx00 0000                       M0-PF   M0-BL
CXM1FB  = $05 ; xx00 0000                       M1-PF   M1-BL
CXBLPF  = $06 ; x000 0000                       BL-PF   -----
CXPPMM  = $07 ; xx00 0000                       P0-P1   M0-M1
INPT0   = $08 ; x000 0000       Read Pot Port 0
INPT1   = $09 ; x000 0000       Read Pot Port 1
INPT2   = $0a ; x000 0000       Read Pot Port 2
INPT3   = $0b ; x000 0000       Read Pot Port 3
INPT4   = $0c ; x000 0000       Read Input (Trigger) 0
INPT5   = $0d ; x000 0000       Read Input (Trigger) 1

        ; RIOT MEMORY MAP

SWCHA   = $280  ; Port A data register for joysticks:
                ; Bits 4-7 for player 1.  Bits 0-3 for player 2.
SWACNT  = $281  ; Port A data direction register (DDR)
SWCHB   = $282  ; Port B data (console switches)
SWBCNT  = $283  ; Port B DDR
INTIM   = $284  ; Timer output

TIMINT  = $285

TIM1T   = $294  ; set 1 clock interval
TIM8T   = $295  ; set 8 clock interval
TIM64T  = $296  ; set 64 clock interval
T1024T  = $297  ; set 1024 clock interval


score   = $80        ; Current score
side    = $81        ; Current side
offset  = $82        ; Current offset
total   = $83        ; Current total
origin  = $84        ; Current origin square
target  = $85        ; Current target square

frame   = $86        ; Current frame

cursorx = $87        ; Current X position of cursor
cursory = $88        ; Current Y position of cursor

pSWCHA  = $89        ; Previous value of SWCHA
pINPT4  = $8A        ; Previous value of INPT4

        ; Reused locations
bitmap0 = $82        ; Index into bitmap (0)
bitmap1 = $83        ; Index into bitmap (1)
even    = $80        ; Marks even/odd

board   = $8c        ; 78 bytes used, there should be space for 12+12+10 bytes of stack

        ;
        ; These are colors for NTSC video, change for PAL
        ;
color_white = $0e    ; Color for white pieces
color_black = $28    ; Color for black pieces
color_white_square = $74     ; Color for white squares
color_black_square = $70     ; Color for black squares

START:
        sei          ; Disable interruptions
        cld          ; Disable decimal mode
        ; Clean up the memory
        lda #0       ; Load zero in accumulator
        tax          ; ...copy in X
sr0:    sta 0,X      ; Save in address 0 plus X
        txs          ; Copy X in S (stack) last value will be $ff
        inx          ; Increment X
        bne sr0      ; Repeat until X is zero.
        sta SWACNT   ; Allow to read joysticks
        sta SWBCNT   ; Allow to read buttons

;       ldx #0       ; x is zero
sr1:    ldy #8
sr3:    lda #$00
        sta board,x
        inx
        dey
        bne sr3
        lda #$07
        sta board,x
        inx
        sta board,x
        inx
        cpx #8*10
        bne sr1
        tax          ; a was $07, so x = $07
sr2:    lda initial,x
        sta board,x
        ora #$08
        sta board+70,x
        inc board+10,x
        lda #$09
        sta board+60,x
        dex
        bpl sr2
        lsr          ; lda #4, but A was $09 / 2 = $04
        sta cursorx
        sta cursory

        ;
        ; Main loop
        ;
sr21:   jsr read_coor
        lda board,y
        and #8          ; Check for white piece
        beq sr21        ; If no, jump and restart selection logic
sr11:   jsr read_coor
        lda board,y
        and #8          ; Check for white piece
        bne sr11        ; If yes, restart target square logic
        jsr sr28        ; Make movement
        ldx #63
kn0:    txa
        lsr
        lsr
        sta AUDV0
        txa
        pha
        jsr kernel
        pla
        tax
        dex
        bne kn0
        jsr play        ; Computer play
        jmp sr21

sr14:   inc offset
        dec total
        bne sr12
sr17:   inx
        cpx #78
        bne sr7
        pla
        tay
        pla
        tsx
        cpx #$ff-2      ; Top call? (2 bytes of return address)
        bne sr24
        ldx score
        cpx #$c0+19     ; Illegal move? (always in check)
        bmi sr24        ; Yes, doesn't move
        tax
sr28:   lda board,x     ; Do move
        cmp #1
        beq sr32
        cmp #9          ; Is it pawn?
        bne sr30
sr32:   cpy #10         ; Reaching border?
        bcc sr31
        cpy #70
        bcc sr30
sr31:   eor #5          ; Make it queen
sr30:   sta board,y
        lda #0          ; Clear origin square
        sta board,x
sr24:   rts

        ;
        ; Computer plays :)
        ;
play:   lda #$c0        ; Current score (-64)
        sta score
        pha             ; Origin square of best movement (currently none)
        pha             ; Target square of best movement
        ldx #0          ; x points to current square
sr7:    lda board,x     ; Read square
        beq sr17        ; Ignore if empty square
        eor side        ; XOR with current playing side
        cmp #7          ; Ignore if frontier
        bcs sr17
        cmp #1          ; Is it pawn?
        bne sr25        ; Carry will be 1 always because 1<=A<=6
        ldy side        ; Is it playing black?
        beq sr25        ; Yes, jump
        lda #0          ; Make it zero for white
sr25:   tay
        adc #3          ; Adds 4 because carry is 1 (see above)
        and #$0c
        sta total       ; Total movements of piece
        lda offsets,y
        sta offset      ; Next offset for movement
sr12:   stx target      ; Restart target square
sr9:    ldy offset
        lda displacement,y
        clc
        adc target      ; Next target square
        sta target
        cmp #78         ; Out of board?
        bcs sr14

        cpy #16
        tay
        lda board,y     ; Content of target square
        beq sr10        ; Jump if empty square
        bcc sr27        ; Jump if isn't not pawn
        lda total
        cmp #3          ; Straight?
        bcc sr17        ; Yes, avoid and cancels any double square movement
sr27:   lda board,y
        eor side
        sec
        sbc #9          ; Valid capture?
        cmp #6
        bcs sr29        ; No, avoid (too far for sr18, use sr29 as bridge)
        cmp #5
        bne sr20        ; Jump if not captured king
        pla             ; Ignore values
        pla
        tsx
        lda #$3f-18     ; Maximum score minus two queens...
        cpx #$f1        ; ...if not in first response.
        bne sr26
        lda #$3f        ; Maximum score (probably checkmate/stalemate)
sr26:   sta score
        rts

sr10:   bcc sr20        ; If isn't pawn, jump.
        lda total
        cmp #2          ; Diagonal?
        beq sr15        ; Jump if one square ahead
sr29:   bcs sr18        ; Yes, avoid
        bcc sr20

sr15:   txa
        ;sec            ; Carry set already because equality comparison
        sbc #20
        cmp #40         ; Moving from center of board?
        bcs sr20
        dec total       ; Yes, then avoid checking for two squares
        ;bcc sr20       ; Fall along

        ; Save all state
sr20:   lda offset      ; Offset for movement
        pha
        lda total       ; Total directions left
        pha
        lda board,y     ; Content of target square
        pha
        tya             ; Target square
        pha
        lda board,x     ; Content of origin square
        pha
        txa             ; Origin square
        sta origin
        pha
        lda board,y
        and #7
        tay
        lda scores,y    ; Score for capture
        tsx
;       cpx #255-10*2+1 ; Depth limit (1-ply)
        cpx #255-10*3+1 ; Depth limit (2-ply)
        bcc sr22
        pha
        lda score       ; Current score
        pha
        ldx origin
        ldy target
        jsr sr28        ; Do move
        lda side
        eor #8          ; Change side
        sta side
        jsr play
        lda side
        eor #8          ; Change side
        sta side
        pla
        tax             ; Current score in x
        pla
        sec             ; Take capture score and substract adversary score
        sbc score
        stx score       ; Restore current score
sr22:   cmp score        ; Better score?
        clc
        bmi sr23        ; No, jump
        bne sr33        ; Better score? yes, jump
        lda frame       ; Equal score, randomize move
        ror
        ror
        jmp sr23        ; No need to update score but carry = 1 will update move
        ;bcc sr23
        ;bcs sr23
sr33:   sta score       ; Update score
        sec
sr23:   pla             ; Restore board
        tax
        pla
        sta board,x
        pla
        sta target
        tay
        pla
        sta board,y
        pla
        sta total
        pla
        sta offset
        bcc sr18
        pla
        pla
        txa             ; Save current best movement
        pha
        tya
        pha

sr18:   lda board,x
        and #7
        cmp #1          ; Was it pawn?
        beq sr16        ; Yes, end sequence, choose next movement
        cmp #5          ; Knight or king?
        bcs sr16        ; End sequence, choose next movement
        lda board,y     ; To empty square?
        bne sr16
        jmp sr9         ; Yes, follow line of squares

sr16:   jmp sr14

        ;
        ; Set object in X
        ; A = X position
        ; First arg = Object to position (0=P0, 1=P1, 2=M0, 3=M1, 4=BALL)
        ; Exits with carry = 0
        ;
        MAC set_x_position
        sta WSYNC       ; 0- Start line synchro
        sec             ; 2- Set carry flag (avoids it in loop)
.AE2:   sbc #15         ; 4- Uses required time dividing A by 15
        bcs .AE2        ; 6/7 - 11/16/21/26/31/36/41/46/51/56/61/66
        tay             ; 8
        lda fine_adjustment-$f1,y ; 13 - Eats 5 cycles crossing page
        sta HMP0+{1}
        nop      
        sta RESP0+{1}     ; 21/26/31/36/41/46/51/56/61/66/71 - "big" positioning
        ENDM

    ;
    ; Display kernel
    ;
kernel:
        lda #$00
        sta COLUBK      ; Background color

        ; VERTICAL_SYNC
        ldx #2
        stx VSYNC       ; Start vertical synchro
        stx WSYNC       ; Wait for 3 lines
        stx WSYNC
        stx WSYNC
        ;
        ldx #43
        stx TIM64T
        sta VSYNC       ; Stop vertical synchro
        sta GRP0
        sta GRP1
        lda #color_black_square
        sta COLUBK      ; Background color
        lda #$35
        sta NUSIZ0      ; Size of player/missile 0
        sta NUSIZ1      ; Size of player/missile 1
        lda #color_white_square
        sta COLUPF      ; Color of playfield
        lda cursorx     ; Get X-position of cursor and set up missile 0
        asl
        asl
        sta even
        asl
        asl
        adc even
        adc #14
        cmp #14
        bne *+4
        sbc #3
        set_x_position 2
        lda frame       ; Update 2 columns of chessboard per frame
        and #3
        asl
        asl
        sta even
        asl
        asl
        adc even
        tax             ; One column for player 0
        bne *+4
        sbc #2
        adc #8
        set_x_position 0
        txa
        ;clc            ; Carry zero already
        adc #88         ; One column for player 1
        set_x_position 1
        sta WSYNC              
        sta HMOVE       ; Fine adjustment for all set_x_position

wait_vblank:
        lda INTIM
        bne wait_vblank
        ;
        ; Start of graphics
        ;
        sta WSYNC              
        sta VBLANK
        sta even        ; Now uses like row counter, start at zero
        lda frame       ; Board position per frame
        and #3
ds0:    tax    
ds1:    sta WSYNC       ; Row 0
        lda even        ; Squares configuration over board
        lsr
        bcc ds6
        lda #$00
        sta PF0
        ldy #$7c
        lda #$f8
        bne ds7

ds6:    lda #$f0
        sta PF0
        ldy #$83
        lda #$07
ds7:    sty PF1
        sta PF2
        lda board,x      ; Check color for the two pieces
        and #8
        beq ds4
        lda #color_white^color_black         ; White for white pieces
ds4:    eor #color_black         ; Green for black pieces
        sta COLUP0
        lda board+4,x
        and #8
        beq ds5
        lda #color_white^color_black         ; White for white pieces
ds5:    eor #color_black         ; Green for black pieces
        sta COLUP1
        sta WSYNC        ; Row 1
        lda even         ; Check if row...
        cmp cursory      ; ...equals row of cursor
        php              ; Save Z flag...
        pla              ; ...so it goes to bit 1
        sta ENAM0        ; Enable missile if at right Y position
        lda board,x      ; Setup pointers to bitmaps for two pieces
        and #7
        asl
        asl
        asl
        sta bitmap0
        lda board+4,x
        and #7
        asl
        asl
        asl              ; //Carry is zero after this instruction
        sta bitmap1
        txa
        pha
        ldy #7           ; Process the 7 lines of bitmap
ds3:    sta WSYNC        ; Row 2/5/8/11/14/17/20
        ldx bitmap0
        lda pieces,x
        sta GRP0
        ldx bitmap1
        lda pieces,x
        sta GRP1
        sta WSYNC       
        sta WSYNC
        inc bitmap0
        inc bitmap1
        dey
        bne ds3
        sty ENAM0        ; Disable cursor
        inc even         ; Increase current row
        pla
        ;clc             ; Carry is still zero//
        adc #10          ; Next row of board
        cmp #80
        bcc ds0

        ;
        ; End of graphics (204 lines)
        ;
        lda #2
        sta WSYNC
        sta VBLANK

        ;
        ; Start overscan timer
        ;
        lda #43                ; 37 lines * 76 = 2812 cycles / 64 = 43.9375
        sta TIM64T
wait_overscan:
        lda INTIM
        bne wait_overscan
        sta WSYNC
        sta PF0
        sta PF1
        sta PF2
        sta WSYNC
        
        inc frame

        rts

        echo "Free bytes section 1: ",$ff00-*

        org $ff00       
fine_adjustment:
        .byte $70       ; -7 
        .byte $60       ; -6 
        .byte $50       ; -5
        .byte $40       ; -4
        .byte $30       ; -3
        .byte $20       ; -2
        .byte $10       ; -1
        .byte $00       ; 0
        .byte $f0       ; +1
        .byte $e0       ; +2
        .byte $d0       ; +3
        .byte $c0       ; +4
        .byte $b0       ; +5
        .byte $a0       ; +6
        .byte $90       ; +7

initial:
        .byte $02,$05,$03,$04,$06,$03,$05,$02

scores:
        .byte 0,1,5,3,9,3

offsets:
        .byte 16,20,8,12,8,0,8

displacement:
        .byte -21,-19,-12,-8,8,12,19,21
        .byte -10,10,-1,1
        .byte 9,11,-9,-11
        .byte -11,-9,-10,-20
        .byte 9,11,10,20

pieces:
        .byte $00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$18,$3c,$3c,$18,$3c,$00,$00
        .byte $5a,$7e,$3c,$3c,$7e,$7e,$00,$00
        .byte $18,$3c,$3c,$3c,$18,$66,$00,$00
        .byte $5a,$5a,$24,$3c,$3c,$3c,$00,$00
        .byte $70,$58,$7c,$6e,$1e,$3e,$00,$00
        .byte $3c,$6a,$56,$3c,$3c,$3c,$00,$00

        ;
        ; Read a coordinate choosen by cursor
        ; Moves y to x, y contains new coordinate.
        ;
read_coor:
        tya
        pha
        jsr read_coor2
        tay
        pla
        tax
        rts

rc5:    ldy #0
        lda SWCHA          ; Read current state of joystick
        sta even
        tax
        eor pSWCHA
        stx pSWCHA         
        eor #$ff
        ora even           ; Disable unchanged directions
        bmi rc0            ; Jump if not going right
        ldx cursorx
        cpx #7
        beq rc0
        inc cursorx
        ldy #8

rc0:    rol                ; Jump if not going left
        bmi rc1
        ldx cursorx
        beq rc1
        dec cursorx
        ldy #8

rc1:    rol                ; Jump if not going down
        bmi rc2
        ldx cursory
        cpx #7
        beq rc2
        inc cursory
        ldy #8

rc2:    rol                ; Jump if not going up
        bmi rc3
        ldx cursory
        beq rc3
        dec cursory
        ldy #8
rc3:    ldx #$01
        stx AUDC0
        sty AUDV0
        sty AUDF0
;       jmp read_coor2     ; Fall thru
        ;
        ; Read a coordinate in a
        ;
read_coor2:
        jsr kernel
        ;lda #0            ; Kernel returns with a = 0
        sta AUDV0
        lda INPT4          ; Read current state of button
        sta even
        tax
        eor pINPT4
        stx pINPT4
        eor #$ff
        ora even           ; Disable unchanged button
        bmi rc5            ; Jump if button not pressed
        ;
        ; Computer plays
        ;
        ldx #$03
        stx AUDC0
        ldx #$08
        stx AUDV0
        stx AUDF0
        lda cursory        ; y_coor 
        asl                ; *2
        asl                ; *4
        adc cursory        ; *5
        asl                ; *10
        adc cursorx        ; + x_coor
        rts

        echo "Free bytes section 2: ",$fffc-*

        org $fffc
        .word START        ; RESET
        .word START        ; BRK
