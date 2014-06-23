   LIST OFF
; ***  R E D  V S  B L U E  ***
; Copyright 1981 Atari, Inc.
; Programmer: Tod Frye

; Analyzed, labeled and commented
;  by Dennis Debro
; Last Update: 25.11.2003

   processor 6502
   include vcs.h
   
   LIST ON   
   
;===============================================================================
; T I A - C O N S T A N T S
;===============================================================================

HMOVE_L7    = $70
HMOVE_L6    = $60
HMOVE_L5    = $50
HMOVE_L4    = $40
HMOVE_L3    = $30
HMOVE_L2    = $20
HMOVE_L1    = $10
HMOVE_0     = $00
HMOVE_R1    = $F0
HMOVE_R2    = $E0
HMOVE_R3    = $D0
HMOVE_R4    = $C0
HMOVE_R5    = $B0
HMOVE_R6    = $A0
HMOVE_R7    = $90
HMOVE_R8    = $80

; values for NUSIZx:
ONE_COPY          = %000
TWO_COPIES        = %001
TWO_MED_COPIES    = %010
TWO_WIDE_COPIES   = %100
THREE_COPIES      = %011
DOUBLE_SIZE       = %101
THREE_MED_COPIES  = %110
QUAD_SIZE         = %111

; values for REFPx:
NO_REFLECT        = %0000
REFLECT           = %1000

;============================================================================
; U S E R - C O N S T A N T S
;============================================================================
      
KERNEL_HEIGHT     = 192          ; height of the display kernel
PLAYER_HEIGHT     = 20           ; height of the players
VBLANK_TIME       = 45           ; vertical blank timeout time
KERNEL_TIME       = 228          ; kernel timeout time
OVERSCAN_TIME     = 34           ; overscan timeout time
ANIMATION_FRAMES  = 8            ; number of animation frames

NUMBER_OF_PLAYERS = 9            ; the number of players

;============================================================================
; C O L O R  C O N S T A N T S
;============================================================================

BLACK    = $00
WHITE    = $0A
GREEN    = $C4
RED      = $36
BLUE     = $16
LT_GREEN = $64

;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================
   SEG.U variables
   org $80
   
gameTimer1           ds 1
gameTimer2           ds 1
gameTimer3           ds 1
gameTimer4           ds 1

   org $86
player0Pointer       ds 2
player1Pointer       ds 2              ; not used
colorPointers        ds 4

   org colorPointers
   
player0ColorPointers ds 2
player1ColorPointers ds 2
animationFrame       ds 1

;===============================================================================
; R O M - C O D E
;===============================================================================

   SEG ROM
   org $F000

Start
   cld
   sei
   ldx #$FF
   txs                              ; set stack to beginning
   inx                              ; x now = 0
   txa
.clear                              ; clears all RAM (does not set VSYNC to 0)
   sta VSYNC,x
   inx
   bne .clear
   
MainLoop
VerticalSync
   lda #$03
   sta WSYNC
   sta VSYNC                        ; start vertical sync (D1 = 1)
   sta WSYNC
   sta WSYNC
   sta WSYNC
   ldx #$00
   stx VSYNC                        ; end vertical sync (D1 = 0)
   lda #VBLANK_TIME
   sta TIM64T

   lda SWCHB                        ; read the console switches
   ror
   ror

; The following is a too complicated time cycle which has no affect on the
; ROM execution. I assume it was here to control the animation frame rate
; but it's not needed. Removing it doesn't alter the animation rate.

   bcs .resetGameTimer3             ; carry bit holds select state

   lda gameTimer3
   bne .incrementGameTimer3
.incrementGameTimer4
   inc gameTimer4
   lda gameTimer4
   cmp #17
   bne .setGameTimer4
   lda #$00
.setGameTimer4
   sta gameTimer4
.incrementGameTimer3
   inc gameTimer3
   lda gameTimer3
   and #$1F
   bne .incrementGameTimer1
   beq .incrementGameTimer4
.resetGameTimer3
   ldy #$00
   sty gameTimer3
.incrementGameTimer1
   inc gameTimer1
   bne GameCalc
   inc gameTimer2
       
GameCalc
   lda #%00000001
   sta CTRLPF                       ; reflect the PF
   
   ldx #$03
.setColorPointers
   lda ColorPointerTbl,x
   sta colorPointers,x
   dex
   bpl .setColorPointers
   
   ldx animationFrame
   lda LowPlayerTable,x
   ldy HiPlayerTable,x
   
   sta player0Pointer
   sta player1Pointer
   
   sty player0Pointer+1
   sty player1Pointer+1
   
   ldx #$06
   sta WSYNC
.coarseMovePlayers
   dex
   bpl .coarseMovePlayers
   sta RESP0                        ; reset the horiz position of GRP0 (pixel 111)
   lda #HMOVE_L4
   sta HMP0                         ; GRP0 now at pixel 107
   lda #WHITE
   sta COLUPF                       ; color the playfield
   sta RESP1                        ; reset the horiz position of GRP1 (pixel 150)
   lda #HMOVE_R6
   sta HMP1                         ; GRP1 now at pixel 156

.vblankWait                         ; loop until vertical blank is done
   lda INTIM
   bne .vblankWait

   lda #KERNEL_TIME
   sta TIM64T                       ; set the timer to end kernel

   lda #$00
   sta WSYNC
;---------------------------------
   sta HMOVE                     ;3
   sta VBLANK                    ;3 = @6  enable TIA output
   
Kernel SUBROUTINE
   lda #GREEN                    ;2
   sta COLUBK                    ;3 = @11 color the background green

   lda #%00000001                ;2
   sta PF2                       ;3 = @16 set the playfield graphic
   sta HMCLR                     ;3 = @19 clear horiz movements >24 cycles since HMOVE :-(
   
   lda #REFLECT                  ;2
   sta REFP1                     ;3 = @24 reflect player1

   ldy #KERNEL_HEIGHT-2          ;2
.kernelLoop
   sta WSYNC
;---------------------------------
   sta HMOVE                     ;3
   lda CopiesOfGRP0,y            ;4
   sta NUSIZ0                    ;3 = @10 -- still in HBLANK
   lda CopiesOfGRP1,y            ;4
   sta NUSIZ1                    ;3 = @17 -- still in HBLANK
   lda (player0Pointer),y        ;5
   sta GRP0                      ;3 = @25
   sta GRP1                      ;3 = @28
   lda (player0ColorPointers),y  ;5
   sta COLUP0                    ;3 = @36
   lda (player1ColorPointers),y  ;5
   sta COLUP1                    ;3 = @44
   lda HorizPosP0,y              ;4
   sta HMP0                      ;3 = @51
   lda HorizPosP1,y              ;4
   sta HMP1                      ;3 = @58
   dey                           ;2
   bne .kernelLoop               ;2³
       
   sta WSYNC
;---------------------------------
   sta HMOVE               ;3
   lda #$00                ;2
   sta GRP0                ;3
   sta GRP1                ;3
   
.wait
   lda INTIM
   bne .wait
   
Overscan SUBROUTINE
   lda #$02
   sta WSYNC
   sta VBLANK                       ; disable TIA output

   lda #OVERSCAN_TIME
   sta TIM64T
   
.wait
   lda INTIM
   bne .wait
   
   lda gameTimer1
   and #$07
   cmp #$07
   bne .nextFrame
   inc animationFrame
   lda #ANIMATION_FRAMES
   cmp animationFrame
   bne .nextFrame
   lda #$00
   sta animationFrame
.nextFrame
   jmp MainLoop

ColorPointerTbl
   .word RedPlayerColors-1, BluePlayerColors-1
   
LowPlayerTable
   .byte <AnimationFrame1-1
   .byte <AnimationFrame2-1
   .byte <AnimationFrame3-1
   .byte <AnimationFrame4-1
   .byte <AnimationFrame5-1
   .byte <AnimationFrame6-1
   .byte <AnimationFrame7-1
   .byte <AnimationFrame8-1

HiPlayerTable
   .byte >AnimationFrame1
   .byte >AnimationFrame2
   .byte >AnimationFrame3
   .byte >AnimationFrame4
   .byte >AnimationFrame5
   .byte >AnimationFrame6
   .byte >AnimationFrame7
   .byte >AnimationFrame8

   .byte $00

PlayerGraphics
AnimationFrame1
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $83;|X.....XX|
   .byte $E2;|XXX...X.|
   .byte $12;|...X..X.|
   .byte $0E;|....XXX.|
   .byte $18;|...XX...|
   .byte $18;|...XX...|
   .byte $3E;|..XXXXX.|
   .byte $30;|..XX....|
   .byte $10;|...X....|
   .byte $18;|...XX...|
   .byte $10;|...X....|
   .byte $18;|...XX...|
   REPEND
   
   org AnimationFrame1 + KERNEL_HEIGHT-1, 0
AnimationFrame2
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $03;|......XX|
   .byte $04;|.....X..|
   .byte $82;|X.....X.|
   .byte $F2;|XXXX..X.|
   .byte $04;|.....X..|
   .byte $30;|..XX....|
   .byte $18;|...XX...|
   .byte $3C;|..XXXX..|
   .byte $38;|..XXX...|
   .byte $10;|...X....|
   .byte $18;|...XX...|
   .byte $10;|...X....|
   .byte $18;|...XX...|
   REPEND
   
   org AnimationFrame2 + KERNEL_HEIGHT-1, 0
AnimationFrame3
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $06;|.....XX.|
   .byte $04;|.....X..|
   .byte $04;|.....X..|
   .byte $84;|X....X..|
   .byte $F4;|XXXX.X..|
   .byte $08;|....X...|
   .byte $38;|..XXX...|
   .byte $30;|..XX....|
   .byte $3C;|..XXXX..|
   .byte $38;|..XXX...|
   .byte $18;|...XX...|
   .byte $0C;|....XX..|
   .byte $08;|....X...|
   .byte $0C;|....XX..|
   REPEND

   org AnimationFrame3 + KERNEL_HEIGHT - 1, 0
AnimationFrame4
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $0C;|....XX..|
   .byte $08;|....X...|
   .byte $40;|.X......|
   .byte $7E;|.XXXXXX.|
   .byte $04;|.....X..|
   .byte $08;|....X...|
   .byte $18;|...XX...|
   .byte $3C;|..XXXX..|
   .byte $38;|..XXX...|
   .byte $18;|...XX...|
   .byte $08;|....X...|
   .byte $0C;|....XX..|
   .byte $08;|....X...|
   .byte $0C;|....XX..|
   REPEND
       
   org AnimationFrame4 + KERNEL_HEIGHT - 1, 0
AnimationFrame5
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $30;|..XX....|
   .byte $00;|........|
   .byte $20;|..X.....|
   .byte $3A;|..XXX.X.|
   .byte $0C;|....XX..|
   .byte $18;|...XX...|
   .byte $18;|...XX...|
   .byte $3A;|..XXX.X.|
   .byte $38;|..XXX...|
   .byte $38;|..XXX...|
   .byte $18;|...XX...|
   .byte $0C;|....XX..|
   .byte $08;|....X...|
   .byte $0C;|....XX..|
   
   REPEAT NUMBER_OF_PLAYERS - 1
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $30;|..XX....|
   .byte $00;|........|
   .byte $02;|......X.|
   .byte $3A;|..XXX.X.|
   .byte $0C;|....XX..|
   .byte $18;|...XX...|
   .byte $18;|...XX...|
   .byte $3A;|..XXX.X.|
   .byte $38;|..XXX...|
   .byte $38;|..XXX...|
   .byte $18;|...XX...|
   .byte $0C;|....XX..|
   .byte $08;|....X...|
   .byte $0C;|....XX..|
   REPEND
   
   org AnimationFrame5 + KERNEL_HEIGHT - 1, 0
AnimationFrame6
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $40;|.X......|
   .byte $80;|X.......|
   .byte $48;|.X..X...|
   .byte $2C;|..X.XX..|
   .byte $1E;|...XXXX.|
   .byte $18;|...XX...|
   .byte $18;|...XX...|
   .byte $50;|.X.X....|
   .byte $5E;|.X.XXXX.|
   .byte $58;|.X.XX...|
   .byte $38;|..XXX...|
   .byte $0C;|....XX..|
   .byte $08;|....X...|
   .byte $0C;|....XX..|
   REPEND

   org AnimationFrame6 + KERNEL_HEIGHT - 1, 0
AnimationFrame7
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $80;|X.......|
   .byte $C0;|XX......|
   .byte $20;|..X.....|
   .byte $16;|...X.XX.|
   .byte $0C;|....XX..|
   .byte $18;|...XX...|
   .byte $18;|...XX...|
   .byte $28;|..X.X...|
   .byte $5C;|.X.XXX..|
   .byte $58;|.X.XX...|
   .byte $30;|..XX....|
   .byte $18;|...XX...|
   .byte $10;|...X....|
   .byte $18;|...XX...|
   REPEND

   org AnimationFrame7 + KERNEL_HEIGHT - 1, 0
AnimationFrame8
   REPEAT NUMBER_OF_PLAYERS
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $00;|........|
   .byte $80;|X.......|
   .byte $C3;|XX....XX|
   .byte $22;|..X...X.|
   .byte $12;|...X..X.|
   .byte $0C;|....XX..|
   .byte $18;|...XX...|
   .byte $30;|..XX....|
   .byte $58;|.X.XX...|
   .byte $50;|.X.X....|
   .byte $30;|..XX....|
   .byte $18;|...XX...|
   .byte $10;|...X....|
   .byte $18;|...XX...|
   REPEND
       
   org AnimationFrame8 + KERNEL_HEIGHT - 2, 0
HorizPosP0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_L7,HMOVE_L7,HMOVE_L2,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_R8
       .byte HMOVE_R8,HMOVE_R8,HMOVE_R8,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_L7,HMOVE_L7,HMOVE_L2,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_L7,HMOVE_L7,HMOVE_L2,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_R8
       .byte HMOVE_R8,HMOVE_R8,HMOVE_R8,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_R4,HMOVE_R8
       .byte HMOVE_R8,HMOVE_R8,HMOVE_R8,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_L6,HMOVE_L6,HMOVE_L6
       .byte HMOVE_L6,HMOVE_L6,HMOVE_L7,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0
       
HorizPosP1
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_R5,HMOVE_R5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_L5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_R5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_L5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_R5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_L5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_L5,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0,HMOVE_0
       .byte HMOVE_0,HMOVE_0,HMOVE_0
       
CopiesOfGRP0
   REPEAT 67
      .byte ONE_COPY
   REPEND
   
   REPEAT 15
      .byte TWO_MED_COPIES
   REPEND
   
   REPEAT 5
      .byte ONE_COPY
   REPEND
   
   REPEAT 15
      .byte TWO_COPIES
   REPEND

   REPEAT 25
      .byte ONE_COPY
   REPEND

   REPEAT 15
      .byte TWO_MED_COPIES
   REPEND

   REPEAT 23
      .byte ONE_COPY
   REPEND
   
   REPEAT 18
      .byte TWO_WIDE_COPIES
   REPEND
   
   REPEAT 8
      .byte ONE_COPY
   REPEND

CopiesOfGRP1
   REPEAT 26
      .byte ONE_COPY
   REPEND

   REPEAT 16
      .byte TWO_MED_COPIES
   REPEND

   REPEAT 64
      .byte ONE_COPY
   REPEND

   REPEAT 16
      .byte TWO_MED_COPIES
   REPEND

   REPEAT 70
      .byte ONE_COPY
   REPEND

RedPlayerColors
   REPEAT NUMBER_OF_PLAYERS
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte WHITE
   .byte WHITE
   .byte WHITE
   .byte WHITE
   .byte WHITE
   .byte WHITE
   .byte RED
   .byte RED
   .byte RED
   .byte RED
   .byte RED
   .byte WHITE
   .byte WHITE
   .byte WHITE
   REPEND

   org RedPlayerColors + KERNEL_HEIGHT - 1, 0
BluePlayerColors       
   REPEAT NUMBER_OF_PLAYERS
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLACK
   .byte BLUE
   .byte BLUE
   .byte BLUE
   .byte BLUE
   .byte BLUE
   .byte BLUE
   .byte LT_GREEN
   .byte LT_GREEN
   .byte LT_GREEN
   .byte LT_GREEN
   .byte LT_GREEN
   .byte WHITE
   .byte WHITE
   .byte WHITE
   REPEND
       
   org $FFFC, 0
   .word Start
   .byte $00,00