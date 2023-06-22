; Author: Lucas Avanço (avanco89 at gmail)
; based on "thin red line by Kirk Israel"
; based on Stella Programming Guide

	processor 6502
	include vcs.h
	org $F000

; Variables (memory region: $80 to $FF)
YPosBall = $80
BallSize = $81
DirectionBall = $82
Top = $83
Down = $84
BallLeftRight = $85
Player0Pos = $86
Player1Pos = $87
Player0Size = $88
Player1Size = $89
Player0ActualSize = $90
Player1ActualSize = $91
LifePlayer0 = $92
LifePlayer1 = $93

Start			; init stuff:
	SEI		; no interruptions
	CLD		; clear BCD Math Bit
	LDX #$FF	; X to up
	TXS		; stack pointer = X
	LDA #0

ClearMem
	STA 0,X		; MEM[X+0] = Accumulator value
	DEX		; X--
	BNE ClearMem

	LDA #190
	STA YPosBall
	LDA #194
	STA Top		; it will allow change ball direction to down
	LDA #6
	STA Down	; it will allow change ball direction to up
	LDA #0
	STA BallSize
	STA DirectionBall	; 0: down, 1: up
	LDA #$10
	STA BallLeftRight	; $10: left, $F0: right
	LDA #110
	STA Player0Pos
	STA Player1Pos
	LDA #0
	STA Player0Size		; draw controller
	STA Player1Size
	LDA #30
	STA Player0ActualSize	; true size
	STA Player1ActualSize
	LDA #30
	STA LifePlayer0
	STA LifePlayer1

SetColors
	LDA #$44
	STA COLUBK	; background color
	LDA #$21	; defining PlayField size (D0) and Ball size (D4, D5)
	STA CTRLPF
	LDA #$10
	STA PF0
	LDA #$10	; ball color
	STA COLUPF

MainLoop
	LDA	#2	; VSYNC D1 = 1 --> turn off electron beam,
			;  position it at the top of the screen
	STA VSYNC	; VSYNC must be sent for at least 3 scanlines
	STA WSYNC	;  by TIA
	STA WSYNC
	STA WSYNC
	LDA #0
	STA VSYNC	; the time is over

	LDA #43		; 37 scanlines * 76 machine cycles = 2812
				; 2812 + 5 cycles(init timer) + 3 cycles(STA WSYNC) + 6 cycles(loop check)
				; Finally we have 2812-14=2798 cycles while VBLANK scanlines, and 2798/64=43
	STA TIM64T

	LDA #2
	STA VBLANK

	; game logic, timer is running
	LDA DirectionBall
	BNE BallUp
	DEC YPosBall
	LDA Down
	CMP YPosBall
	BNE EndBallUpDown
BallUp
	LDA #1
	STA DirectionBall
	INC YPosBall
	LDA Top
	CMP YPosBall
	BNE EndBallUpDown
	LDA #0
	STA DirectionBall
EndBallUpDown
	; input control Player0: down and up
	LDA #%00010000	;Up?
	BIT SWCHA 
	BNE SkipMoveDown0
	INC Player0Pos
	INC Player0Pos
SkipMoveDown0
	LDA #%00100000	;Down?
	BIT SWCHA 
	BNE SkipMoveUp0
	DEC Player0Pos
	DEC Player0Pos
SkipMoveUp0
	; input control Player1: down and up
	LDA #%00000001	;Up?
	BIT SWCHA 
	BNE SkipMoveDown1
	INC Player1Pos
	INC Player1Pos
SkipMoveDown1
	LDA #%00000010	;Down?
	BIT SWCHA 
	BNE SkipMoveUp1
	DEC Player1Pos
	DEC Player1Pos
SkipMoveUp1
	;check collision: Player0 and Ball
	LDA #%1000000
	BIT CXP0FB
	BEQ NoCollisionP0Ball
	LDA #$10	; ball must go left
	STA BallLeftRight
NoCollisionP0Ball
	;check collision: Player1 and Ball
	LDA #%1000000
	BIT CXP1FB
	BEQ NoCollisionP1Ball
	LDA #$F0	; ball must go right
	STA BallLeftRight
NoCollisionP1Ball
	;check collision: Ball and PlayField
	LDA #%10000000
	BIT CXBLPF
	BEQ NoCollisionBallPF
	STA CXCLR	; clear all collisions
	LDX #$10
	CPX BallLeftRight	; check ball direction and decide who fail and will suffer punishment
	BEQ Player1Penalty
Player0Penalty
	DEC Player0ActualSize
	DEC LifePlayer0
	BEQ EndGame
	JMP NoCollisionBallPF
Player1Penalty
	DEC Player1ActualSize
	DEC LifePlayer1
	BEQ EndGame
	JMP NoCollisionBallPF
NoCollisionBallPF
	STA CXCLR	; clear all collisions
	JMP WaitVBlankEnd
EndGame
	JMP Start

WaitVBlankEnd
	LDA INTIM	; load timer
	BNE WaitVBlankEnd	; killing time if the timer != 0
	; 37 VBLANK scanlines has gone
	STA WSYNC
	STA RESP1
	
	LDY #191	; count scanlines
	STA WSYNC	; wait for scanline end, we do not wanna begin at the middle of one
	STA VBLANK	; VBLANK D1 = 0

	LDA BallLeftRight	; speed
	STA HMBL
	LDA #$00			; speed
	STA GRP0			; D0 to D7 are considered
	STA GRP1
	STA WSYNC
	STA HMOVE	; strobe register like WSYNC

ScanLoop
	STA WSYNC
; check if we are at ball position, scanline
	CPY YPosBall
	BEQ ActiveBallSize
	LDA BallSize
	BNE DrawingBall
NoBall
	LDA #0
	STA ENABL
	JMP OutBall
ActiveBallSize
	LDA #8
	STA BallSize
DrawingBall
	LDA #2
	STA ENABL
	DEC BallSize
OutBall
; check Player0 position
	CPY Player0Pos
	BEQ ActivePlayer0Size
	LDA Player0Size
	BNE DrawingPlayer0
NoPlayer0
	LDA #0
	STA GRP0
	JMP OutPlayer0
ActivePlayer0Size
	LDA Player0ActualSize
	STA Player0Size
DrawingPlayer0
	LDA #2
	STA GRP0
	DEC Player0Size
OutPlayer0
; check Player1 position
	CPY Player1Pos
	BEQ ActivePlayer1Size
	LDA Player1Size
	BNE DrawingPlayer1
NoPlayer1
	LDA #0
	STA GRP1
	JMP OutPlayer1
ActivePlayer1Size
	LDA Player1ActualSize
	STA Player1Size
DrawingPlayer1
	LDA #2
	STA GRP1
	DEC Player1Size
OutPlayer1


	DEY
	BNE ScanLoop

	LDA #2
	STA WSYNC
	STA VBLANK	; turn it on, actual tv picture has gone

	; Overscan
	LDX #30
OverScanWait
	STA WSYNC
	DEX
	BNE OverScanWait

	JMP MainLoop

; Kirk Israel words:
; OK, last little bit of crap to take care of.
; there are two special memory locations, $FFFC and $FFFE
; When the atari starts up, a "reset" is done (which has nothing to do with
; the reset switch on the console!) When this happens, the 6502 looks at
; memory location $FFFC (and by extension its neighbor $FFFD, since it's 
; seaching for both bytes of a full memory address)  and then goes to the 
; location pointed to by $FFFC/$FFFD...so our first .word Start tells DASM
; to put the binary data that we labeled "Start" at the location we established
; with org.  And then we do it again for $FFFE/$FFFF, which is for a special
; event called a BRK which you don't have to worry about now.
	org $FFFC
	.word Start
	.word Start
