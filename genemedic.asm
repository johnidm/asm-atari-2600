; This is Gene Medic version 1.0
; by Jason H. Moore, Ph.D.

; File genemedic.asm
; Last updated 12/29/17

; Compile with DASM genemedic.asm -ogenemedic.bin -f3

; The goal of the game is to save a patient with a common
; disease such as cancer by editing mutations in their
; genes. Mutations can have known or unknown consequences upon
; editing (joystick down) with some revealed by consulting the 
; electronic health record powerup (computer) or the scientific 
; literature (text) power ups (joystick up). Each gene edit costs
; money that can be replenished with the money bag power up.
; The goal is to improve the patient health ("P" top score) 
; before the financial resources are exhausted ("$" lower score).

; For more information see http://GeneMedic.org

; Special thanks to Andrew Davies and Darrell Spice for their
; great tutorials that helped with the development of this
; edutainment game. Thanks to countless others who posted
; useful comments and code on various websites and forums
; including the Stella mailing list and AtariAge.com.
; I also found the book "Making games for the Atari 2600"
; by Steven Hugg to be useful. I have tried to comment the code
; as much as I possibly can without writing my own book.
; Programming for the 2600 is not easy! You have been warned :)

; ****************************************************************
; Tell the assembler we are working with the 6502 processor
; ****************************************************************

	PROCESSOR 6502

; ****************************************************************
; Include the vcs header file with the 2600 register and
; memory map definitions. Also include the macro file that
; has some useful functions such as 'sleep' used here.
; ****************************************************************

	INCLUDE VCS.h
	INCLUDE macro.h

; ****************************************************************
; Constants set here
; ****************************************************************

P0HEIGHT   = 55     ; height of player 0 sprite in scanlines
PUHEIGHT   = 16     ; height of powerup sprite in scanlines
SCHEIGHT   = 14     ; height of the score playfield
CYTHEIGHT  = 14     ; height of the cytoplasm playfield
DNAHEIGHT  = 11     ; height of the DNA helix playfield
HHEIGHT    = 36     ; height of the heart playfield
GHEIGHT    = 14     ; height of the gene name playfield
JHEIGHT    = 14     ; height of the jockey name playfield

XMAX	   = 150    ; max X position to right for player
XMIN	   = 5      ; min X position to left for player

P0XSTART   = 20	    ; player 0 starting X position
P0YSTART   = 80	    ; player 0 starting Y position

P1RESX     = 38     ; player 1 starting X position for research 
		    ; powerup - in sleep cycles
P1MONX     = 54     ; player 1 starting X position for money
		    ; bag powerup - in sleep cycles
P1EHRX     = 22     ; player 1 starting X position for EHR
		    ; powerup - in sleep cycles

M0X        = 28     ; missile 0 starting X position for in sleep
		    ; cycles. This M is used to queue position
                    ; of nucleotide to edit triggering tricorder
                    ; graphic above player 0.

; ****************************************************************
; Tell the assembler the memory locations of our game variables.
; ds is a pseudo-op for assembler that stands for 'define space'
; ds 1 = one byte and ds 2 = two bytes. Remember that the 2600
; only has 128 bytes of RAM!
; ****************************************************************

	SEG.U variables ; pseudo-op for the assembler
   	ORG $80         ; the starting location of RAM

P0X        	ds 1    ; player 0 X coordinate
P0Y         	ds 1 	; player 0 Y coordinate
P0Pos		ds 1    ; byte for P0 fine (high nibble) and			
			; course (low nibble) horizontal position
P0LineColor	ds 1    ; player 0 color slice for that scanline

P0GfxFlag	ds 1	; flag for which P0 graphic to use
			; 1 = no tricorder 
			; 2 = health tricorder [+]
			; 3 = harm tricorder [!]
			; 4 = unknown tricorder [-]
			; 5 = context dependent tricorder [::]

ResearchFlag	ds 1	; flag for drawing the research powerup
			; 1 = draw, 0 = don't draw
EHRFlag		ds 1	; flag for drawing the EHR powerup
			; 1 = draw, 0 = don't draw
MoneyFlag	ds 1	; flag for drawing the money bag powerup
			; 1 = draw, 0 = don't draw
NewLevelFlag    ds 1	; time to draw a new playfield? This gets set
			; to 1 when player reaches right side of screen
ScreenFlag	ds 1	; which screen to draw? A or B?
                        ; this help set up alternating screens with
                        ; different playfield graphics for sense
                        ; of movement through the cell

PScore		ds 1	; this is the patient health score
DScore		ds 1	; this is the dollars score
DScoreFLag	ds 1	; flag to indicate whether DScore = 0
                        ; to prevent going negative
ScoreGfx	ds 1	; holds the current score graphics line

UpFlag		ds 1	; flag for joystick moved up (for power ups)
DownFlag	ds 1	; flag for joystick moved down (for edits)

MutationFlag	ds 1	; holds info about nature of mutation
                        ; set each screen by a random number
			; 0 = no mutation
			; 1 = mutation - known, helpful
			; 2 = mutation - known, harmful
			; 3 = mutation - unknown, helpful
			; 4 = mutation - unknown, harmful
			; 5 = mutation - context dependent, helpful
			; 6 = mutation - context dependent, harmful

EditFlag        ds 1    ; keeps track of successful edits
                        ; so score only modified once per screen

MBUsedFlag      ds 1	; keeps track of whether money bag powerup
                        ; has been used for that screen. Only used once.

WinFlag		ds 1	; this indicates that the player has won the game
LoseFlag	ds 1	; this indicates that the player has lost the game

EndSoundFlag    ds 1    ; flag indicating final sound effects are done
                        ; so they only play once at end of game

RandomPU	ds 1	; holds 8-bit pseudo random number for powerups

RandomMut	ds 1	; holds 8-bit pseudo random number for mutations

GameLaunchFlag	ds 1	; a flag for indicating the game has booted.
                        ; this is used to indicate the start screen
                        ; should be drawn until fire button pressed

SFX_LEFT        ds 1    ; index for sound effect functions
SFX_RIGHT       ds 1    ; index for sound effect functions

; ****************************************************************
; Tell the assembler where the origin (ORG) of the program is
; ROM is memory location $F000 to $FFFF
; ****************************************************************

	SEG             ; pseudo-op to set current segment
	ORG $F000       ; let the assembler know start address

; ****************************************************************
; This initializes the RAM and TIA registers by writing 0s.
; It also initalizes the stack pointer to $FF. This code comes
; from Andrew Davie's tutorial. Note just clearing the RAM did
; not work well on the original 2600 hardware.
; ****************************************************************

Reset

	ldx #0    	; load the X register with a 0
	txa             ; transfer X to accumulator

Clear

	dex             ; decrement X register
	txs             ; transfer X to stack pointer
	pha             ; push accumulator onto stack
	bne Clear       ; branch to Clear if accumulator != 0

; ****************************************************************
; This initializes the random seed for the GetRandom subroutine
; Using two different random numbers to prevent dependencies
; between powerups and mutations. 
; ****************************************************************

SetRandomSeed

	lda INTIM  	; this is an unknown value
        eor #$FF	; XOR to flip bits so seed is not 0
	sta RandomPU    ; Seed for GetRandomPU

	lda INTIM  	; this is an unknown value
        eor #$FF	; XOR to flip bits so seed is not 0
	sta RandomMut   ; Seed for GetRandomMut


; ****************************************************************
; This initializes the game launch flag for drawing start screen
; when game is first booted.
; ****************************************************************

	lda #1  	    ; load accumulator with a 1
	sta GameLaunchFlag  ; set game launch flag to draw start
                            ; screen first time through until
                            ; reset switch is pressed then flag=0
        jmp PlayfieldInit   ; bypass the next bit of code on boot

; ****************************************************************
; This is marks the place in the code to jump to when starting a
; game after fire button is pressed from start screen.
; ****************************************************************

StartHere      

	lda #0     	    ; load accumulator with a 0
	sta GameLaunchFlag  ; reset game launch flag so start
                            ; screen isn't drawn anymore after
                            ; reset switch is pressed 

; ****************************************************************
; This initializes the player and playfield color registers
; ****************************************************************

PlayfieldInit    

	lda #$80	; load accumulator with dark blue
	sta COLUBK	; store in the background color memory

	lda #1		; load the accumulator with 1
	sta CTRLPF	; storing a 1 here indicates a 
			; reflected playfield

	lda #$1E	; Load the player 1 color
	sta COLUP1      ; set player 1 color register

; ****************************************************************
; This initializes the player X and Y position.
; ****************************************************************

	lda #P0XSTART	; load accumulator with X position
	sta P0X		; set P0 starting X position

	lda #P0YSTART	; load accumulator with Y position
	sta P0Y		; set P0 starting Y position

; ****************************************************************
; This initializes the patient (top) and money (bottom) scores
; that are presented to the player as colors. Brighter yellow for
; the patient score is an indication of health. Brighter yellow 
; for the money score means more resources have been spent on the
; patient. The game is over when the max patient score (win), min
; patient score (loss), or max money score (loss) are reached.
; ****************************************************************

	lda #1		; load accumulator with 1
	sta PScore	; set left score to 1

	lda #1		; load accumulator with 1
	sta DScore	; set left score to 1

; ****************************************************************
; This initializes the game flags for tracking score and powerups.
; Most are set to 0 with only th P0 graphics flag set to 1.
; ****************************************************************

	lda #1		  ; load accumulator with flag = 1 to start
	sta P0GfxFlag	  ; set player 0 graphic flag = 1	
		          ; this draws player without tricorder.
                          ; tricorder comes on when P0 collides 
                          ; with M0 that is present when mutation 
			  ; flag > 0
	
	lda #0		  ; load accumulator with flag = 0 to start
	sta UpFlag        ; set joystick Up Flag = 0
	sta DownFlag      ; set joystick Down Flag = 0
	sta EHRFlag       ; set EHR Flag = 0
	sta MoneyFlag     ; set Money Bag Flag = 0
	sta ResearchFlag  ; set Research Flag = 0
	sta ScreenFlag    ; set Screen Flag = 0
	sta EditFlag      ; set Edit Flag = 0 to start, no edit
	sta MBUsedFlag    ; set Money Bag used Flag = 0 to start
	sta WinFlag       ; set the Win Flag = 0 to start, 1=win
	sta LoseFlag      ; set the Lose Flag = 0 to start, 1=lose
	sta MutationFlag  ; set Flag = 0, no mutation first screen
	sta EndSoundFlag  ; set Flag = 0 until end of game
	sta NewLevelFlag  ; time to draw new playfield? This gets 
			  ; set to 1 when player reaches right side 
			  ; of screen
	
; ****************************************************************
; The first part of a 2600 program is the vertical sync that
; tells the TV to get ready to draw a picture. The CPU needs
; to wait for three scanlines for the TV to get ready. This
; is done with VSYNC to start this process and WSYNC (x2) to
; wait for the scanlines to finish.
; ****************************************************************

VerticalSYNC       	   ; The start of the new frame 
		   	   ; Main game loop starts here

	lda #2     	   ; load the accumulator with a 2
	sta VSYNC  	   ; store 2 in VSYNC memory to start

	sta WSYNC  	   ; wait for scanline to finish
	sta WSYNC  	   ; second WSYNC (need at least 2)
	
	lda #0     	   ; load the accumulator with a 0
	sta VSYNC  	   ; store 0 in VSYNC memory to end

; ****************************************************************
; The second part of a 2600 program is the vertical blank
; with an additional 37 scanlines of time before the screen
; is actually drawn on the TV. Can do some game computations
; here but must count the scanlines to add up to 37 total.
; 37 scanlines at 76 cycles each works out to be 43 ticks of
; the timer. Will set TIM64T to 43 to keep track of cycles.
; Will use this time for horizontal positioning of the sprites.
; Will also use this time for game calculations.
; ****************************************************************

	lda #43		   ; we have 43 ticks of the timer here
	sta TIM64T	   ; start the timer for VBLANC

GameOverCheck1		   ; check win & lose flags for game over
                           ; if game over skip game calculations
                           ; so no player movement during final screen

        lda WinFlag        ; load accumulator with Win Flag
        beq GameOverCheck2 ; check lose flag if win flag = 0
	jmp VerticalBLANK  ; skip game calcs if flag = 1

GameOverCheck2

	lda LoseFlag       ; load accumulator with Lose Flag
        beq NewLevelCheck  ; move on to game calcs if lose flag = 0
	jmp VerticalBLANK  ; skip game calcs if flag = 1

NewLevelCheck 		   ; time to draw new level with new random
			   ; powerup and gameplay flags?

	lda NewLevelFlag   ; if flag = 1 then it is a new level
	bne NewLevel       ; if flag = 0 then do game calculations
	jmp CheckWin       ; but skip setting new gameplay flags
                           ; using jmp here to avoid error due to
	                   ; a branch that is too long.
			   ; the new level flag gets set to 1 when
			   ; the player gets to far right side of PF

NewLevel                   ; start here when it is a new level
                           ; and new flags need to be set
                           ; e.g. new powerup flags get set here

SetEditFlag                ; set this flag to 0 on new level

	lda #0             ; load accumulator with a 0
	sta EditFlag       ; make sure Edit Flag = 0 on new screen
                           ; this flag prevents multiple changes to 
                           ; score on same screen.
	sta MBUsedFlag	   ; reset this flag that keeps track of 
                           ; whether the money bag powerup was used

	sta WSYNC          ; wait for finish of scanline

SetScreenFlag		   ; set the flag used to track which screen
                           ; is drawn for cyto and nuc parts of PF
                           ; this flag alternates between 0 and 1

	lda ScreenFlag     ; load accumulator with flag value
        beq ScreenFlagSet1 ; flag is zero so need to change to 1  

ScreenFlagSet0		   ; flag = 1 so change to a 0

        lda #0		   ; load accumulator with a 0
        sta ScreenFlag     ; flag is 1 so need to change to 0
        jmp SetPUFlags     ; done setting helix flag
        
ScreenFlagSet1		   ; change this flag to a 1
	
	lda #1		   ; load accumulator with a 1
        sta ScreenFlag     ; flag is 0 so need to change to 1

SetPUFlags                 ; set new flags for the powerups

	lda #0             ; make sure all flags are reset to 0
                           ; when a new level starts
	sta EHRFlag        ; set flag to 0
	sta ResearchFlag   ; set flag to 0
	sta MoneyFlag      ; set flag to 0

	jsr GetRandomPU    ; generate a random number from 0-255
                           ; number is returned in accumulator
	and #%00000011	   ; limit number to 0-3
        tax                ; transfer accumulator to X register
	sta WSYNC          ; wait for finish of scanline

CheckEHR                   ; display EHR powerup?

        cpx #0             ; if Random=0 then set EHR flag to 1
        bne CheckRes	   ; if != 1 check research powerup
	lda #1             ; load accumulator with a 1
	sta EHRFlag        ; set flag = 1
	sta WSYNC          ; wait for finish of scanline 

CheckRes                   ; display research powerup?

	cpx #1             ; if Random=1 then set research flag to 1
        bne CheckMon	   ; now check money bag powerup
	lda #1             ; load accumulator with a 1
	sta ResearchFlag   ; set flag = 1
	sta WSYNC          ; wait for finish of scanline

CheckMon                   ; display money bag powerup?

	cpx #2             ; if Random=2 then set money bag flag to 1
        bne CheckDone	   ; done setting powerup flags
	lda #1             ; load accumulator with a 1
	sta MoneyFlag      ; set flag = 1
	sta WSYNC          ; wait for finish of scanline

CheckDone                  ; if Random=3 then no powerup
                           ; on that screen and all powerup
                           ; flags = 0

	lda #0		   ; load accumulator with a 0
	sta NewLevelFlag   ; set this flag = 0 to reset this flag
                           ; no longer a new level

SetMutFlags                ; set new flags for the mutation

	lda #0             ; make sure flag is reset to 0
	sta MutationFlag   ; set flag to 0

	jsr GetRandomMut   ; generate a random number from 0-255
                           ; number is returned in accumulator
	and #%00000111     ; limit number to 0-7
	sta MutationFlag   ; store value in mutation flag
        
	sta WSYNC          ; wait for finish of scanline

CheckWin                   ; is the game over? winner?

	ldx PScore         ; load the X register with the patient score
        cpx #9             ; is the patient healed?
	bne CheckLose1     ; branch if PScore != 9, not healed

        lda #1             ; load accumulator with a 1
        sta WinFlag        ; winner! Set WinFlag = 1

	jmp VerticalBLANK  ; move on to draw final screen

CheckLose1                 ; check for loss based on patient score = 0

	sta WSYNC          ; wait for finish of scanline

	lda PScore         ; load accumulator with the patient score
	bne CheckLose2     ; branch if PScore != 0

        lda #1             ; load accumulator with a 1
        sta LoseFlag       ; loser! Set LoseFlag = 1

	jmp VerticalBLANK  ; move on to draw final screen

CheckLose2                 ; check for loss based on dollar score = 9

	ldx DScore         ; load the X register with the patient score
        cpx #9             ; is the player out of money?
	bne MinDScore      ; branch if DScore != 9

        lda #1             ; load accumulator with a 1
        sta LoseFlag       ; loser! Set LoseFlag = 1

	jmp VerticalBLANK  ; move on to draw final screen

MinDScore                  ; this keep the DScore from falling below 1

	lda DScore         ; load X register with DScore value      
        bne SetM0X         ; branch if X != 0
       
	lda #1             ; load accumulator with a 1
        sta DScore         ; store the 1 in DScore to prevent 0

SetM0X			   ; setting the X position for missile 0

	sleep M0X	   ; sleep this many cycles to set M0 
                           ; in middle of screen
	sta RESM0	   ; trigger X position for missile 0
                           ; this never changes and is used as
                           ; position marker for player 0
	sta WSYNC	   ; wait for finish of scanline 

   			   ; next section sets the horizontal
                           ; position of player 1 with unique
 			   ; X positions for each powerup.
		           ; no fine positioning needed here
			   ; because P1 is static

SetP1XEHR		   ; set powerup X pos if EHR flag = 1
	
	lda EHRFlag        ; load the accumulator with EHR flag
	beq SetP1XRes      ; jump to next if flag = 0

	sleep P1EHRX       ; delay to horizontally position sprite
	sta RESP1          ; set sprite X pos by writing to RESP1 

	sta WSYNC	   ; wait for finish of scanline

	jmp StartHMoveP0   ; go to next section for P0 positioning

SetP1XRes		   ; set powerup X pos if research flag = 1

	sta WSYNC	   ; wait for finish of scanline
	
	lda ResearchFlag   ; load the accumulator with research flag
	beq SetP1XMon      ; jump to next if flag = 0
	
	sleep P1RESX       ; delay to horizontally position sprite
	sta RESP1          ; set sprite X pos by writing to RESP1
 
	sta WSYNC	   ; wait for finish of scanline

	jmp StartHMoveP0   ; go to next section for P0 positioning

SetP1XMon		   ; set powerup X pos if money bag flag = 1

	sta WSYNC	   ; wait for finish of scanline
	
	lda MoneyFlag      ; load the accumulator with money bag flag
	beq StartHMoveP0   ; jump to next if flag = 0
	
	sleep P1MONX       ; delay to horizontally position sprite
	sta RESP1          ; set sprite X pos by writing to RESP1
 
StartHMoveP0		   ; process P0 horizontal move here
			   ; this bit of code from Battlezone game
 			   ; commonly used for horizontal positioning

	sta WSYNC	   ; wait for finish of scanline
			   ; timing of this WSYNC and next important
			   ; for correct horizontal movement
	
	lda P0X		   ; load accumulator with P0 X position

	sec		   ; set the carry flag

DivideLoop		   ; divide by 15		   
	
	sbc #15		   ; subtract 15
	bcs DivideLoop	   ; branch until negative

	eor #7		   ; calculate the fine offset
	asl		   ; arithmetic shift left
	asl
	asl
	asl

	sta RESP0	   ; fix the course position
	sta WSYNC	   ; wait for finish of scanline
	sta HMP0 	   ; set the fine offset

CheckMoneyBag              ; is the money bag powerup activated?

	lda MoneyFlag      ; check if money bag powerup present
        beq CheckP0M0Col1  ; if=0 then powerup not present

	lda UpFlag         ; is the jostick up?
	beq CheckP0M0Col1  ; if=0 then joystick not up and skip

	lda MBUsedFlag     ; has the money bag powerul been used?
	beq MBUse          ; branch to use money bag if flag = 0 

	jmp CheckP0M0Col1  ; jump because powerup already used

MBUse

	dec DScore         ; powerup activated - decrease DScore
	lda #1		   ; load accumulator with a 1
	sta MBUsedFlag     ; store the 1 in MBUsedFlag so can't use
	
	ldy #sfxPING       ; select ping sound effect 
        jsr SFX_TRIGGER    ; execute the sound

CheckP0M0Col1              ; This checks for a P0 - M0 collision

	lda #1		   ; load the accumulator with a 1
	sta P0GfxFlag      ; set the P0 graphics flag to 
			   ; tricorder off

	lda #%01000000	   ; load the D6 bit in accumulator
	bit CXM0P          ; bit compare to what is in col register
	bne Collision1     ; branch if collision = 1

        jmp VerticalBLANK  ; skip to Vertical BLANK if no collision
                           ; using jmp here because branch too long

Collision1  
	
	lda MutationFlag   ; load accumulator with mutation flag
	tax		   ; transfer accumulator to X register

	cpx #7             ; compare X register to 7
	bne Collision2     ; branch if != 7

	ldx #5             ; change the 7 to a 5
			   ; need to do something with the 7 since
                           ; we only have 6 choices to map below
                           ; this creates a slight excess of context
                           ; dependent mutations that are helpful
			   ; this is ok because I like these best

Collision2

	lda DownFlag       ; load accumulator with down joystick flag
	tay		   ; transfer accumulator to Y register

        cpx #0             ; is there a mutation? no mutation = 0
        bne MutationYes    ; if yes process type of mutation

	jmp MutationDone   ; if no jump to mutation done and VBLANK

MutationYes

	sta WSYNC	   ; wait for finish of scanline

MutKnownPos		   ; known helpful mutation?

	cpx #1             ; is mutation flag = 1?
	bne MutKnownNeg    ; branch if flag != 1

	lda #2             ; load the accumulator with 2
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right tricorder visible

	lda EditFlag       ; load edit flag to see if edit already made
	beq MKPEdit        ; branch if edit = 0 

        jmp MutationDone   ; otherwise we are done
                          
MKPEdit			   ; let's edit if joystick is down

	cpy #1             ; is joystick down? Did a tay earlier
        bne MutKnownNeg    ; skip if not

	inc PScore         ; helpful editing has occured, inc score
	inc DScore         ; money used to edit, inc score

	ldy #sfxHELP       ; select sound effect for helpful edit
        jsr SFX_TRIGGER    ; execute the sound
	
	lda #1             ; load accumulator with a 1
	sta EditFlag       ; reset edit flag = 1 to indicate no more
			   ; edits this level

	sta WSYNC	   ; wait for finish of scanline

	jmp MutationDone   ; all done - move on to VBLANK

MutKnownNeg		   ; known harmful mutation?

	cpx #2             ; is mutation flag = 2?
	bne MutUnknownPos  ; branch if flag != 2

	lda #3             ; load the accumulator with 3
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right tricorder visible

	lda EditFlag       ; load edit flag to see if edit already made
	beq MKNEdit        ; branch if edit = 0 

        jmp MutationDone   ; otherwise we are done
                          
MKNEdit			   ; let's edit if joystick is down

	cpy #1             ; is joystick down?
        bne MutUnknownPos  ; skip if not

	dec PScore         ; harmful editing has occured, dec score
	inc DScore         ; money used to edit, inc score

	ldy #sfxHARM       ; select sound effect for harmful edit
        jsr SFX_TRIGGER    ; execute the sound
	
	lda #1             ; load accumulator with a 1
	sta EditFlag       ; reset edit flag = 1 to indicate no more
			   ; edits this level

	sta WSYNC	   ; wait for finish of scanline

	jmp MutationDone   ; all done - move on to VBLANK

MutUnknownPos		   ; unknown positive mutation - player must guess
                           ; or use research powerup to reveal effect

	cpx #3             ; is mutation flag = 3?
	bne MutUnknownNeg  ; branch if flag != 3

	lda ResearchFlag   ; now check if research powerup present
        beq MUPP04         ; if=0 then powerup not present
                           ; and player doesn't know type of mutation

	lda UpFlag         ; is the jostick up?
	beq MUPP04         ; if=0 then not up and draw tricorder 4 
                           ; with symbol for unknown mutation

	lda #2             ; load the accumulator with 2
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right helpful tricorder visible

	jmp MUPP02         ; skip the next section

MUPP04                     ; draw the neutral tricorder since
                           ; mutation unknown to player

	lda #4             ; load the accumulator with 4
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right tricorder visible

MUPP02

	lda EditFlag       ; load edit flag to see if edit already made
	beq MUPEdit        ; branch if edit = 0 

        jmp MutationDone   ; otherwise we are done
                          
MUPEdit			   ; let's edit if joystick is down

	cpy #1             ; is joystick down?
        bne MutUnknownNeg  ; skip if not

	inc PScore         ; helpful editing has occured, inc score
	inc DScore         ; money used to edit, inc score

	ldy #sfxHELP       ; select sound effect for helpful edit
        jsr SFX_TRIGGER    ; execute the sound
	
	lda #1             ; load accumulator with a 1
	sta EditFlag       ; reset edit flag = 1 to indicate no more
			   ; edits this level

	sta WSYNC	   ; wait for finish of scanline

	jmp MutationDone   ; all done - move on to VBLANK

MutUnknownNeg		   ; unknown positive mutation - player must guess
                           ; or use research powerup to reveal effect

	cpx #4             ; is mutation flag = 4?
	bne MutContextPos  ; branch if flag != 4

	lda ResearchFlag   ; now check if research powerup present
        beq MUNP04         ; if=0 then powerup not present
                           ; and player doesn't know type of mutation

	lda UpFlag         ; is the jostick up?
	beq MUNP04         ; if=0 then not up and draw tricorder 4 
                           ; with symbol for context-dependent mutation

	lda #3             ; load the accumulator with 3
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right helpful tricorder visible

	jmp MUNP03         ; skip the next section

MUNP04                     ; draw the neutral tricorder since
                           ; mutation unknown to player

	lda #4             ; load the accumulator with 4
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right tricorder visible

MUNP03

	lda EditFlag       ; load edit flag to see if edit already made
	beq MUNEdit        ; branch if edit = 0 

        jmp MutationDone   ; otherwise we are done
                          
MUNEdit			   ; let's edit if joystick is down

	cpy #1             ; is joystick down?
        bne MutContextPos  ; skip if not

	dec PScore         ; harmful editing has occured, dec score
	inc DScore         ; money used to edit, inc score

	ldy #sfxHARM       ; select sound effect for harmful edit
        jsr SFX_TRIGGER    ; execute the sound
	
	lda #1             ; load accumulator with a 1
	sta EditFlag       ; reset edit flag = 1 to indicate no more
			   ; edits this level

	sta WSYNC	   ; wait for finish of scanline

	jmp MutationDone   ; all done - move on to VBLANK


MutContextPos		   ; context-dependent positive mutation 
                           ; player must guess or use EHR powerup

	cpx #5             ; is mutation flag = 5?
	bne MutContextNeg  ; branch if flag != 5

	lda EHRFlag        ; now check if EHR powerup present
        beq MCPP05         ; if=0 then powerup not present
                           ; and player doesn't know type of mutation

	lda UpFlag         ; is the jostick up?
	beq MCPP05         ; if=0 then not up and draw tricorder 5 
                           ; with symbol for context-depentent mutation

	lda #2             ; load the accumulator with 2
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right helpful tricorder visible

	jmp MCPP02         ; skip the next section

MCPP05                     ; draw the neutral tricorder since
                           ; mutation unknown to player

	lda #5             ; load the accumulator with 5
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right tricorder visible

MCPP02

	lda EditFlag       ; load edit flag to see if edit already made
	beq MCPEdit        ; branch if edit = 0 

        jmp MutationDone   ; otherwise we are done
                          
MCPEdit			   ; let's edit if joystick is down

	cpy #1             ; is joystick down?
        bne MutContextNeg  ; skip if not

	inc PScore         ; helpful editing has occured, inc score
	inc DScore         ; money used to edit, inc score

	ldy #sfxHELP       ; select sound effect for helpful edit
        jsr SFX_TRIGGER    ; execute the sound
	
	lda #1             ; load accumulator with a 1
	sta EditFlag       ; reset edit flag = 1 to indicate no more
			   ; edits this level

	sta WSYNC	   ; wait for finish of scanline

	jmp MutationDone   ; all done - move on to VBLANK 

MutContextNeg		   ; context-dependent negative mutation 
                           ; player must guess or use EHR powerup

	cpx #6             ; is mutation flag = 6?
	bne MutationDone   ; branch to done if flag != 6
                           ; this allow nothing to happen when flag = 7

	lda EHRFlag        ; now check if EHR powerup present
        beq MCNP05         ; if=0 then powerup not present
                           ; and player doesn't know type of mutation

	lda UpFlag         ; is the jostick up?
	beq MCNP05         ; if=0 then not up and draw tricorder 5 
                           ; with symbol for context-depentent mutation

	lda #3             ; load the accumulator with 3
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right helpful tricorder visible

	jmp MCNP03         ; skip the next section

MCNP05                     ; draw the neutral tricorder since
                           ; mutation unknown to player

	lda #5             ; load the accumulator with 5
	sta P0GfxFlag      ; set the P0 graphics flag
			   ; to have right tricorder visible

MCNP03

	lda EditFlag       ; load edit flag to see if edit already made
	beq MCNEdit        ; branch if edit = 0 

        jmp MutationDone   ; otherwise we are done
                          
MCNEdit			   ; let's edit if joystick is down

	cpy #1             ; is joystick down?
        bne MutationDone   ; skip if not

	dec PScore         ; harmful editing has occured, dec score
	inc DScore         ; money used to edit, inc score

	ldy #sfxHARM       ; select sound effect for harmful edit
        jsr SFX_TRIGGER    ; execute the sound
	
	lda #1             ; load accumulator with a 1
	sta EditFlag       ; reset edit flag = 1 to indicate no more
			   ; edits this level

	sta WSYNC	   ; wait for finish of scanline

MutationDone	           ; all done checking mutations

	sta CXCLR	   ; clear the collision registers
	sta WSYNC	   ; wait for finish of scanline
		
VerticalBLANK              ; The finish vertical blank loop
			   ; This burns the scanlines left over

	lda INTIM	   ; load timer value into accumulator
	bne VerticalBLANK  ; loop if timer not zero

	sta WSYNC          ; wait for scanline to finish
	sta HMOVE	   ; finalize the horizontal move of P0
			   ; this triggers execution of motion
                           ; degree of move controlled by HMP0/1
                           ; must be done at this point in code

	sta WSYNC          ; wait for scanline to finish

	sta VBLANK         ; store 0 in VBLANK memory to end
                           ; and start drawing the picture by
                           ; turning the TV beam on	

; ****************************************************************
; The third part of a 2600 program is the screen drawing
; routine that is often called the kernel. This is where
; the players, missiles, and playfield are drawn. There
; are 192 scanlines to be drawn with 228 color clocks per line.
; There are 192 * 228 = 43,776 color clocks. There are 3
; machine cycles per color clock. This give 14,592 machine
; cycles. Dividing this by 64 gives 228 timer ticks. So, we store
; 228 in TIM64T to start the timer that is used to draw screen.
; We are using the Y register to keep track of 192 scanlines.
; We will draw the screen in chunks starting with the scores.
; ****************************************************************

	ldy #192           ; load the Y register with 192
			   ; this is the first scanline at top
			   ; of the screen
	lda #228	   ; 228 timer ticks
	sta TIM64T	   ; start the timer

	lda #$00	   ; load accumulator with PF color
	sta COLUPF	   ; set the PF color to black

	lda #%11111111     ; draw solid pattern for initial PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register

DrawScreenTop              ; Draw very top part of screen in black
	
	dey		   ; decrement Y to burn a scanline 
	sta WSYNC          ; wait for scanline to finish
	cpy #188	   ; Have 4 score scanlines passed?
	bne DrawScreenTop  ; loop until Y = 188 then set up the
			   ; score part of PF

	ldx PScore         ; Load X with current patient score
        lda PScoreColor,x  ; use the score to get the PF color
	sta COLUPF	   ; score-based color of PF for patient

	dey		   ; burn scanline 187 to create border
	sta WSYNC          ; wait for scanline to finish

	ldx (#SCHEIGHT-1)  ; set up the score line counter

DrawPScore                 ; Draw score for patient health

	lda PatientGfx,x   ; Load patient graphics for this line
	sta PF1		   ; Draw the "P" for patient

	SLEEP 21	   ; kill time so this is only part of PF
			   ; drawn

	lda BlankGfx1,x    ; Blank graphics to erase 2nd PF1
	sta PF1		   ; Draw the blank PF1 graphics

	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	cpy #173	   ; Have 14 score scanlines passed?

	sta WSYNC          ; wait for scanline to finish

	bne DrawPScore     ; loop until Y = 173 then set up the
			   ; dollar score part of PF

	dey		   ; burn scanline 172 to create border
	sta WSYNC          ; wait for scanline to finish

	lda #$00	   ; load accumulator with PF color
	sta COLUPF	   ; set the PF color to black

DrawDivider1               ; Draw 4 scanline black divider between 
			   ; the patient and dollar scores 
	
	dey		   ; decrement the Y register 
	sta WSYNC          ; wait for scanline to finish
	cpy #168	   ; Have 4 score scanlines passed?
	bne DrawDivider1   ; loop until Y = 168 then draw the
			   ; dollar score part of PF

	ldx DScore         ; Load X with current Left Score
        lda DScoreColor,x  ; use the score to get the PF color
	sta COLUPF	   ; score-based color of PF for patient

	sleep 16	   ; kill some cycles here to keep the PF
			   ; in sync

	ldx (#SCHEIGHT-1)  ; set up the score line counter

	dey		   ; burn scanline 167 to create border
	sta WSYNC          ; wait for scanline to finish

DrawDScore                 ; Draw score for dollars

	lda DollarGfx,x    ; Load dollar graphics for this line
	sta PF1		   ; Draw the "D" for dollar

	SLEEP 20	   ; kill time so this is only part of PF
			   ; drawn

	lda BlankGfx1,x    ; Blank graphics to erase 2nd PF1
	sta PF1		   ; Draw the blank PF1 graphics

	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	cpy #153	   ; Have 14 score scanlines passed?

	sta WSYNC          ; wait for scanline to finish

	bne DrawDScore     ; loop until Y = 153 then set up the
			   ; dollar score part of PF

	dey		   ; burn scanline 152 to create border
	sta WSYNC          ; wait for scanline to finish

	lda #$00	   ; load accumulator with PF color
	sta COLUPF	   ; set the PF color to black

DrawDivider2               ; Draw 4 scanline black divider between 
			   ; scores and the rest of the playfield
	
	dey		   ; decrement the Y register 
	sta WSYNC          ; wait for scanline to finish
	cpy #148	   ; Have 4 score scanlines passed?
	bne DrawDivider2   ; loop until Y = 148 then draw the
			   ; dollar score part of PF

	lda #$F0	   ; load accumulator with powerup bar color
	sta COLUPF	   ; set the PF color

	dey		   ; burn scanline 147
	sta WSYNC          ; wait for scanline to finish

DrawPowerBar       	   ; draw the next section that displays
			   ; research, money bag, and EHR powerups

CheckResearch              ; check the research powerup flag

	ldx ResearchFlag   ; load the accumulator with res flag
	cpx #0             ; compare X to 0
	beq CheckEHRFlag   ; skip drawing this powerup if X=0

	ldx (#PUHEIGHT-1)  ; get line count to draw player 1

DrawResearch

	lda #$0E           ; set the research powerup color to white
	sta COLUP1         ; write the color to the P1 color register
	lda Research,x     ; load player 1 (object) graphic
	sta GRP1           ; set the graphic for this line
	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #131	   ; allow 16 scanlines to draw powerups
	bne DrawResearch   ; loop back to continue drawing powerup
			   ; part of screen 
        jmp PowerDrawDone  ; done drawing powerups

CheckEHRFlag               ; check the EHR powerup flag

	ldx EHRFlag        ; load the accumulator with EHR flag
	cpx #0             ; compare X to 0
	beq CheckMoneyFlag ; skip drawing this powerup if X=0

	ldx (#PUHEIGHT-1)  ; get line count to draw player 1

DrawEHR

	lda #$FC           ; set the EHR powerup color to orange
	sta COLUP1         ; write the color to the P1 color register
	lda EHR,x          ; load player 1 (object) graphic
	sta GRP1           ; set the graphic for this line
	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #131	   ; allow 16 scanlines to draw powerups
	bne DrawEHR        ; loop back to continue drawing powerup
			   ; part of screen 
        jmp PowerDrawDone  ; done drawing powerups

CheckMoneyFlag             ; check the money bag powerup flag

	ldx MoneyFlag      ; load accumulator with money bag flag
	cpx #0             ; compare X to 0
	beq PowerDrawDone  ; skip drawing this powerup if X=0

	ldx (#PUHEIGHT-1)  ; get line count to draw player 1

DrawMoney

	lda #$CA           ; set the money powerup color to green
	sta COLUP1         ; write the color to the P1 color register
	lda MoneyBag,x     ; load player 1 (object) graphic
	sta GRP1           ; set the graphic for this line
	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #131	   ; allow 16 scanlines to draw powerups
	bne DrawMoney      ; loop back to continue drawing powerup
			   ; part of screen
	
PowerDrawDone

	lda #0		   ; load accumulator with a 0
	sta GRP1	   ; zero out player 1 graphics

DrawDivider3               ; Draw 3 scanline divider between 
			   ; scores and the rest of the playfield

	dey		   ; burn a scanline 
	sta WSYNC          ; wait for scanline to finish
	cpy #128	   ; Have 3 score scanlines passed?
	bne DrawDivider3   ; loop until Y = 128 then draw the
			   ; cell membrane part of PF

	lda #$02	   ; load accumulator with cell mem color
	sta COLUPF	   ; set the PF color

DrawCellMem                ; Draw cell membrane loop
			   ; PF registers already set from above

	dey	           ; decrement the Y register
	cpy #120	   ; Have 8 scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawCellMem    ; loop until Y = 120 then set up the
			   ; pattern for the cytoplasm part of PF

	lda #$80	   ; load accumulator with cyto color
	sta COLUPF	   ; set the PF color

DrawCytTop      	   ; draw the next section that represents
			   ; the cytoplasm part of the cell PF
                           ; draw top part first
	
	dey	           ; decrement the Y register
	cpy #110  	   ; Have 10 scanlines passed?
	sta WSYNC          ; wait for scanline to finish
        bne DrawCytTop     ; this is the first part of cyoplasm PF

	ldx (#CYTHEIGHT-1) ; set up the cytoplasm line counter

	dey		   ; decrement the Y register
	sta WSYNC          ; burn scanline 109 to save a few cycles
                           ; so we can draw asymmetrical PF next

	lda #$48	   ; load accumulator with cyto color
	sta COLUPF	   ; set the PF color

DrawCytOrgs                ; Draw the cytoplasmic organelles next

	lda ScreenFlag     ; flag for which screen to draw
        beq DrawOrgB       ; if flag = 0 branch to organelles B

DrawOrgA		   ; draw organelles option A

	lda BlankGfx2,x    ; Load blank graphics for this line
	sta PF0		   ; Draw the blank line

	lda MitoGfx,x      ; Load mitochondria graphics for this line
	sta PF1		   ; Draw the mitochondria line

	lda BlankGfx2,x    ; Load blank graphics for this line
	sta PF2		   ; Draw the blank line

	SLEEP 18	   ; kill time for asymmetric playfield
                           ; so we can draw both mito and ER

	lda ERGfx,x        ; Load ER graphics for this line
	sta PF1		   ; Draw the ER line
	jmp DrawOrgDone    ; skip the next section

DrawOrgB		   ; draw organelles option B

	lda BlankGfx2,x    ; Load blank graphics for this line
	sta PF0		   ; Draw the blank line

	lda ERGfx,x        ; Load mitochondria graphics for this line
	sta PF1		   ; Draw the mitochondria line

	lda BlankGfx2,x    ; Load blank graphics for this line
	sta PF2		   ; Draw the blank line

	SLEEP 18	   ; kill time for asymmetric playfield
                           ; so we can draw both mito and ER

	lda MitoGfx,x      ; Load ER graphics for this line
	sta PF1		   ; Draw the ER line

DrawOrgDone		   ; done drawing the organelles

	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	sta WSYNC          ; wait for scanline to finish
	cpy #95 	   ; Have 14 score scanlines passed?
	bne DrawCytOrgs    ; loop until Y = 96 then set up the
			   ; dollar score part of PF

	lda #$80	   ; load accumulator with cyto color
	sta COLUPF	   ; set the PF color

	lda #%11111111     ; draw solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register

DrawCytBot

	dey	           ; decrement the Y register
	cpy #85  	   ; Have 10 scanlines passed?
	sta WSYNC          ; wait for scanline to finish
        bne DrawCytBot     ; this is the bottom part of cyoplasm PF

	lda #$02	   ; load accum with nucleur membrane color
	sta COLUPF	   ; set the PF color

DrawNucMem       	   ; draw the next section that represents
			   ; the nuclear membrane part of the cell PF
	
	dey	           ; decrement the Y register
	cpy #81 	   ; Have 4 scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawNucMem     ; loop until Y = 81 then set up the
			   ; pattern for the nucleus part of PF

	lda #$70	   ; load accumulator with nucleus color
	sta COLUPF	   ; set the PF color 

	ldx P0GfxFlag	   ; load accumulator with value for flag

StartScreenSwitch          ; check the game launch flag to see if
                           ; the start screen should be drawn

	lda GameLaunchFlag ; load accumulator with game launch flag
        beq WinSwitch      ; if flag = 0 then don't draw new screen
	ldx (#GHEIGHT-1)   ; load X register with gene line count
        jmp DrawNuc8       ; if flag = 1 then draw start screen

WinSwitch                  ; if win flag set draw final PF

	lda WinFlag        ; load accumulator with flag value
        beq LoseSwitch     ; if flag = 0 branch to next
	ldx (#HHEIGHT-1)   ; load X register with line count
        jmp DrawNuc6       ; if flag = 1 then draw win screen

LoseSwitch                 ; if lose flag set draw final PF

	lda LoseFlag       ; load accumulator with flag value
        beq P0GfxSwitch1   ; if flag = 0 branch to next
	ldx (#HHEIGHT-1)   ; load X register with line count
        jmp DrawNuc7       ; if flag = 1 then draw win screen

P0GfxSwitch1		   ; this set of switches determine what
			   ; graphics to use for player 0
	cpx #1             ; compare X register to 1 
        bne P0GfxSwitch2   ; if not = 1 then branch to next 
	ldx (#P0HEIGHT-1)  ; load X register with P0 line count
	dey		   ; burn scanline 80 to save some cycles
	sta WSYNC          ; for drawing first line of player 0
	jmp DrawNuc1       ; go draw nucleus 1 with P0Graphic1    

P0GfxSwitch2

	cpx #2             ; compare X register to 2 
        bne P0GfxSwitch3   ; if not = 2 then branch to next 
	ldx (#P0HEIGHT-1)  ; load X register with P0 line count
	dey		   ; burn scanline 80 to save some cycles
	sta WSYNC          ; for drawing first line of player 0   
	jmp DrawNuc2       ; go draw nucleus 2 with P0Graphic2

P0GfxSwitch3

	cpx #3             ; compare X register to 3 
        bne P0GfxSwitch4   ; if not = 3 then branch to next  
	ldx (#P0HEIGHT-1)  ; load X register with P0 line count
	dey		   ; burn scanline 80 to save some cycles
	sta WSYNC          ; for drawing first line of player 0 
	jmp DrawNuc3       ; go draw nucleus 3 with P0Graphic3

P0GfxSwitch4

	cpx #4             ; compare X register to 4 
        bne P0GfxSwitch5   ; if not = 4 then branch to next 
	ldx (#P0HEIGHT-1)  ; load X register with P0 line count  
	dey		   ; burn scanline 80 to save some cycles
	sta WSYNC          ; for drawing first line of player 0 
	jmp DrawNuc4       ; go draw nucleus 4 with P0Graphic4

P0GfxSwitch5

	cpx #5             ; compare X register to 5 
        bne SwitchDone     ; if not = 5 then branch to next
	ldx (#P0HEIGHT-1)  ; load X register with P0 line count
	dey		   ; burn scanline 80 to save some cycles
	sta WSYNC          ; for drawing first line of player 0 
	jmp DrawNuc5       ; go draw nucleus 5 with P0Graphic5

SwitchDone 
	
	dey		   ; burn scanline 80 to save some cycles
	sta WSYNC          ; for drawing first line of player 0    
	
DrawNuc1                   ; Draw the nucleus screen loop with P0
			   ; graphics 1 (no tricorder)
	
	lda P0Graphic1,x   ; load player 0 graphic 1
	sta GRP0

	lda P0Color,x	   ; grab the color of P0 for this line
	sta COLUP0         ; set color for P0 each scanline

	lda #0             ; load accumulator with 0
	sta ENAM0	   ; make sure missile 0 is off until later

MissileOn1 		   ; this bit turns on missile 0 at the
			   ; right scanline. Used to check for
			   ; collision with P0 for turning on
			   ; tricorder for editing the DNA

	cpy #47  	   ; check for scanline 47
	bne SkipMissile1   ; skip if not right scanline
	lda #2             ; load accumulator with 2
	sta ENAM0          ; use the 2 to turn on missile 0
	
SkipMissile1               ; skip turning on the missile

	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #25 	   ; time to draw the DNA PF?
	bne DrawNuc1       ; loop back to continue drawing nucleus
			   ; part of screen with PF and player

	jmp DrawDNAPrep	   ; done drawing P0 - let's draw the DNA
			   ; part of the playfield

DrawNuc2                   ; Draw the nucleus screen loop with P0
			   ; graphics 1 (no tricorder)
	
	lda P0Graphic2,x   ; load player 0 graphic 2
	sta GRP0

	lda P0Color,x	   ; grab the color of P0 for this line
	sta COLUP0         ; set color for P0 each scanline

	lda #0             ; load accumulator with 0
	sta ENAM0	   ; make sure missile 0 is off until later

MissileOn2 		   ; this bit turns on missile 0 at the
			   ; right scanline. Used to check for
			   ; collision with P0 for turning on
			   ; tricorder for editing the DNA

	cpy #47  	   ; check for scanline 47
	bne SkipMissile2   ; skip if not right scanline
	lda #2             ; load accumulator with 2
	sta ENAM0          ; use the 2 to turn on missile 0
	
SkipMissile2

	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #25 	   ; time to draw the DNA PF?
	bne DrawNuc2       ; loop back to continue drawing nucleus
			   ; part of screen with PF and player

	jmp DrawDNAPrep	   ; done drawing P0 - let's draw the DNA
			   ; part of the playfield

DrawNuc3                   ; Draw the nucleus screen loop with P0
			   ; graphics 3 (tricorder harm !)

	lda P0Graphic3,x   ; load player 0 graphic 3
	sta GRP0

	lda P0Color,x	   ; grab the color of P0 for this line
	sta COLUP0         ; set color for P0 each scanline

	lda #0             ; load accumulator with 0
	sta ENAM0	   ; make sure missile 0 is off until later

MissileOn3 		   ; this bit turns on missile 0 at the
			   ; right scanline. Used to check for
			   ; collision with P0 for turning on
			   ; tricorder for editing the DNA

	cpy #47  	   ; check for scanline 47
	bne SkipMissile3   ; skip if not right scanline
	lda #2             ; load accumulator with 2
	sta ENAM0          ; use the 2 to turn on missile 0
	
SkipMissile3

	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #25 	   ; time to draw the DNA PF?
	bne DrawNuc3       ; loop back to continue drawing nucleus
			   ; part of screen with PF and player

	jmp DrawDNAPrep	   ; done drawing P0 - let's draw the DNA
			   ; part of the playfield

DrawNuc4                   ; Draw the nucleus screen loop with P0
			   ; graphics 4 (unknown tricorder)

	lda P0Graphic4,x   ; load player 0 graphic 4
	sta GRP0

	lda P0Color,x	   ; grab the color of P0 for this line
	sta COLUP0         ; set color for P0 each scanline

	lda #0             ; load accumulator with 0
	sta ENAM0	   ; make sure missile 0 is off until later

MissileOn4 		   ; this bit turns on missile 0 at the
			   ; right scanline. Used to check for
			   ; collision with P0 for turning on
			   ; tricorder for editing the DNA

	cpy #47  	   ; check for scanline 47
	bne SkipMissile4   ; skip if not right scanline
	lda #2             ; load accumulator with 2
	sta ENAM0          ; use the 2 to turn on missile 0
	
SkipMissile4

	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #25 	   ; time to draw the DNA PF?
	bne DrawNuc4       ; loop back to continue drawing nucleus
			   ; part of screen with PF and player

	jmp DrawDNAPrep	   ; done drawing P0 - let's draw the DNA
			   ; part of the playfield

DrawNuc5                   ; Draw the nucleus screen loop with P0
			   ; graphics 5 (context-dependent tricorder)

	lda P0Graphic5,x   ; load player 0 graphic 5
	sta GRP0

	lda P0Color,x	   ; grab the color of P0 for this line
	sta COLUP0         ; set color for P0 each scanline

	lda #0             ; load accumulator with 0
	sta ENAM0	   ; make sure missile 0 is off until later

MissileOn5 		   ; this bit turns on missile 0 at the
			   ; right scanline. Used to check for
			   ; collision with P0 for turning on
			   ; tricorder for editing the DNA

	cpy #47  	   ; check for scanline 47
	bne SkipMissile5   ; skip if not right scanline
	lda #2             ; load accumulator with 2
	sta ENAM0          ; use the 2 to turn on missile 0
	
SkipMissile5

	dex                ; decrease the line counter	
	dey 		   ; decrease the scanline count
	sta WSYNC	   ; wait for scanline to finish
	cpy #25 	   ; time to draw the DNA PF?
	bne DrawNuc5       ; loop back to continue drawing nucleus
			   ; part of screen with PF and player
	jmp DrawDNAPrep	   ; done drawing P0 - let's draw the DNA
			   ; part of the playfield

DrawNuc6                   ; this draws the final screen with
                           ; a colored heart for healed patient (red)
	                
	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #71	           ; Have 10 DNA scanlines passed?

	sta WSYNC          ; wait for scanline to finish

	bne DrawNuc6       ; loop until Y = 71 then set up the
			   ; pattern for heart PF

DrawHeartWin

	lda #$30	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for rest of PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	lda HeartGfx,x     ; load the heart graphics for this line
	sta PF2		   ; set the PF2 register 
	
	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	cpy #35	           ; Have 24 DNA scanlines passed?

	sta WSYNC          ; wait for scanline to finish

	bne DrawHeartWin   ; loop until Y = 47 then set up the
			   ; pattern for last part of nucleaus PF

FinHeartWin

	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #23	           ; On scanline 23?
	sta WSYNC          ; wait for scanline to finish
	bne FinHeartWin    ; loop until Y = 23 then set up the
			   ; DNA PF

	jmp DrawDNAPrep	   ; done drawing P0 - let's draw the DNA
			   ; part of the playfield

DrawNuc7                   ; this draws the final screen with
                           ; a colored heart for dead patient (grey)

	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #71	           ; Have 10 DNA scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawNuc7       ; loop until Y = 71 then set up the
			   ; pattern for heart PF

DrawHeartLose

	lda #$06	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for rest of PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	lda HeartGfx,x     ; load the heart graphics for this line
	sta PF2		   ; set the PF2 register 
	
	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	cpy #35	           ; Have 24 DNA scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawHeartLose  ; loop until Y = 47 then set up the
			   ; pattern for last part of nucleaus PF

FinHeartLose

	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #23	           ; On scanline 23?
	sta WSYNC          ; wait for scanline to finish
	bne FinHeartLose   ; loop until Y = 23 then set up the
			   ; DNA PF

	jmp DrawDNAPrep    ; get ready draw DNA helix

DrawNuc8                   ; this draws the start screen with
                           ; the name of the game on boot up
	                
	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #70	           ; Have 10 DNA scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawNuc8       ; loop until Y = 70 then set up the
			   ; pattern for heart PF

	;lda #$0E	   ; load the accumulator with color number
	;sta COLUPF	   ; store in the playfield color memory

DrawGeneName               ; draw "Gene" on playfield

	lda GeneColor,x    ; load the colors for the name
	sta COLUPF         ; write the colors to the register

	lda Gene1Gfx,x     ; load gene name graphics for this line
	sta PF1		   ; set the PF1 register
	lda Gene2Gfx,x     ; load gene name graphics for this line
	sta PF2		   ; set the PF2 register

	sleep 10            ; burn cycles for asymmetrical playfield

	lda BlankGfx2,x    ; load gene name graphics for this line
	sta PF1		   ; set the PF1 register
	lda BlankGfx2,x    ; load gene name graphics for this line
	sta PF2		   ; set the PF2 register
	
	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	cpy #56	           ; Have 14 DNA scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawGeneName   ; loop until Y = 56 then set up the
			   ; pattern for last part of nucleaus PF

DrawNameGap                ; this draws some blank PF lines between

	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #52	           ; On scanline 52?
	sta WSYNC          ; wait for scanline to finish
	bne DrawNameGap    ; loop until Y = 52 then draw the
			   ; jockey name part of playfield

	ldx (#JHEIGHT-1)   ; load X register with jockey line count

DrawMedicName              ; draw "Medic" on playfield

	lda MedicColor,x   ; load the colors for the name
	sta COLUPF         ; write the colors to the register

	lda Medic1Gfx,x    ; load medic name graphics for this line
	sta PF1		   ; set the PF1 register
	lda Medic2Gfx,x    ; load medic name graphics for this line
	sta PF2		   ; set the PF2 register

	sleep 11            ; burn cycles for asymmetrical playfield

	lda BlankGfx2,x    ; load gene name graphics for this line
	sta PF1		   ; set the PF1 register
	lda BlankGfx2,x    ; load gene name graphics for this line
	sta PF2		   ; set the PF2 register 

	dex	
	dey	           ; decrement the Y register
	cpy #38	           ; On scanline 38?
	sta WSYNC          ; wait for scanline to finish
	bne DrawMedicName  ; loop until Y = 38 then draw remaining
                           ; blank lines of nucleus

FinDrawNames               ; finish drawing the start screen nucleus

	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	lda #%00000000     ; solid pattern for PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 
	
	dey	           ; decrement the Y register
	cpy #23	           ; On scanline 23?
	sta WSYNC          ; wait for scanline to finish
	bne FinDrawNames   ; loop until Y = 23 then draw the
			   ; DNA helix part of playfield

DrawDNAPrep   		   ; Time to get ready to draw the DNA helix



	ldx (#DNAHEIGHT-1) ; set up the DNA line counter

	dey		   ; decrement the scanline count
	sta WSYNC	   ; burn scanline 24 to save a few cycles
                           ; for drawing the helices

	lda #$1E	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

	
	
DrawDNA

	lda ScreenFlag     ; flag for which screen to draw
	beq DrawHelixB     ; if flag = 0 branch to draw helix B

DrawHelixA		   ; draw helix option A

	lda HelixA0,x      ; Load helix graphics for this line
	sta PF0		   ; Draw helix A
	lda HelixA1,x      ; Load helix graphics for this line
	sta PF1		   ; Draw helix A
	lda HelixA2,x      ; Load helix graphics for this line
	sta PF2		   ; Draw helix A
	jmp HelixDone      ; done loading helix graphics

DrawHelixB		   ; draw helix option B

	lda HelixB0,x      ; Load helix graphics for this line
	sta PF0		   ; Draw helix B
	lda HelixB1,x      ; Load helix graphics for this line
	sta PF1		   ; Draw helix B
	lda HelixB2,x      ; Load helix graphics for this line
	sta PF2		   ; Draw helix B

HelixDone		   ; done loading helix graphics
	
	dex                ; decrement our line counter
	dey	           ; decrement the Y register
	cpy #13	           ; Have 11 DNA scanlines passed?
	sta WSYNC          ; wait for scanline to finish
	bne DrawDNA        ; loop until Y = 11 then set up the
			   ; pattern for bottom strand of DNA PF

	lda #%11111111     ; solid pattern for rest of PF
        sta PF0		   ; set the PF0 register
	sta PF1		   ; set the PF1 register
	sta PF2		   ; set the PF2 register 

	lda #$80	   ; load the accumulator with color number
	sta COLUPF	   ; store in the playfield color memory

DrawBot		           ; draw the very bottom part of screen
			   ; until timer runs out

	sta WSYNC	   ; wait for the scanline to finish
	lda INTIM	   ; check timer count
	bne DrawBot        ; loop if timer not 0
         
	lda #2             ; load accumulator with 2 binary 0010
	sta VBLANK         ; turn off screen for overscan

; ****************************************************************
; The final part of a 2600 program is the overscan that waits a
; final 30 scanlines to finish one complete frame on TV screen
; We will use this time to check for joystick movement. Note
; that this game is side to side only. No up or down movement.
; SWCHA data bits D7 to D4 = 1 for P0 right, left, down, up
; D3 to D0 are used for P1. Also checking for game reset switch
; and firebutton for starting a game. Also doing win/lose sound 
; here.
; ****************************************************************

	lda #35           ; load the X register with 35 ticks
	sta TIM64T	  ; initialize the timer

GameResetCheck            ; has the reset switch been pressed?
                          ; if so branch up to Reset to clear
                          ; memory and reset the whole game

	lda SWCHB         ; load accumulator with state of reset
                          ; switch
	and #%00000001    ; reset game? and returns a 1 if so
	bne SoundUpdate   ; move on if no reset switch
        jmp Reset         ; if reset switch pushed then reboot game

SoundUpdate               

	jsr SFX_UPDATE    ; update sound effects

MoveLeft

	lda #%01000000	  ; D6 = 1 for P0 stick right
	bit SWCHA         ; bit compare to SWCHA register
	bne MoveRight	  ; check move right if no left joystick

	ldx P0X		  ; load X register with P0 X position
	cpx #XMIN	  ; is P0 at left edge of screen?
	beq MoveRight	  ; if so left move check is done

	dec P0X		  ; otherwise decrease the P0 X position

	lda #%00001000	  ; a 1 in D3 of REFP0 says mirror
	sta REFP0	  ; reflection of P0 in mirror image

MoveRight

	lda #%10000000	  ; D7 = 1 for P0 stick right
	bit SWCHA         ; bit compare to SWCHA register
	bne StickUp	  ; moving to StickUp if no right joystick

	ldx P0X		  ; load X register with P0 X position
	cpx #XMAX	  ; is P0 at right edge of screen?
	beq AtEdge	  ; if so move to AtEdge to set flag
                          ; and set P0X = XMIN

	inc P0X		  ; otherwise increase the P0 X position
	
	lda #%00000000	  ; a 0 in D3 of REFP0 says no mirror
	sta REFP0	  ; no reflection of P0
	jmp StickUp       ; bypass the new level flag for stick up

AtEdge                    ; this gets executed if end of PF reached

	lda #1            ; load accumulator with a 1
        sta NewLevelFlag  ; set flag = 1 to indicate new level
                          ; needs to be drawn. Gets checked in 
			  ; vertical blank above.
	lda #XMIN         ; load accumulator with X min value
	sta P0X           ; set the P0 horizontal location to XMIN
	
StickUp

	lda #0            ; load the accumulator with a 0
	sta UpFlag        ; zero out the stick up flag

	lda #%00010000	  ; D4 = 1 for P0 stick up
	bit SWCHA         ; bit compare to SWCHA register
	bne StickDown	  ; moving to StickDown if no up joystick

	lda #1		  ; load the accumulator with a 1
        sta UpFlag  	  ; indicate with flag that stick up
	
StickDown

	lda #0            ; load the accumulator with a 0
	sta DownFlag      ; zero out the stick down flag

	lda #%00100000	  ; D3 = 1 for P0 stick down
	bit SWCHA         ; bit compare to SWCHA register
	bne MoveDone	  ; moving done if no down joystick

	lda #1		  ; load the accumulator with a 1
        sta DownFlag  	  ; indicate with flag that stick down

MoveDone	          ; done checking joysticks for movement

	lda EndSoundFlag  ; only play end sounds once if Flag=0
	bne StartButton   ; skip end game sounds if Flag=1

WinSound                  ; Game over? Play win sound effects

	lda WinFlag       ; load accumulator with win flag
        beq LoseSound     ; branch if win flag not set

	ldy #sfxWIN1      ; sound effect 1 for win
        jsr SFX_TRIGGER   ; execute the sound

	ldy #sfxWIN2      ; sound effect 2 for win
        jsr SFX_TRIGGER   ; execute the sound

	ldy #sfxWIN3      ; sound effect 3 for win
        jsr SFX_TRIGGER   ; execute the sound	

	lda #1            ; load accumulator with a 1
        sta EndSoundFlag  ; set Flag=1 so sound done

	jmp StartButton   ; skip the lose sound check

LoseSound                 ; Game over? Play loss sound effects

	lda LoseFlag      ; load accumulator with lose flag
        beq StartButton   ; branch if lose flag not set

	ldy #sfxLOSE1     ; sound effect 1 for loss
        jsr SFX_TRIGGER   ; execute the sound

	ldy #sfxLOSE2     ; sound effect 2 for loss
        jsr SFX_TRIGGER   ; execute the sound

	ldy #sfxLOSE3      ; sound effect 3 for loss
        jsr SFX_TRIGGER   ; execute the sound		

	lda #1            ; load accumulator with a 1
        sta EndSoundFlag  ; set Flag=1 so sound done	

StartButton               ; check if button pressed on startup screen

        lda GameLaunchFlag ; load the game launch flag
        beq OverScan       ; branch to OverScan when flag = 0  

	lda INPT4         ; load accumulator with button value
        bmi OverScan      ; If INPT4 D7 = 1 then continue
        jmp StartHere     ; If INPT4 D7 = 0 then button pressed
                          ; jumpt to Start Here to start new game       

OverScan                  ; The overscan loop

	lda INTIM	  ; Load the timer into the accumulator
	bne OverScan	  ; Loop if timer not yet zero

	jsr GetRandomPU   ; ping the random number generators once
	jsr GetRandomMut  ; per frame to make the sequence less
			  ; predictable when needed by the player
			  ; crossing a mutation or new screen.

	sta WSYNC         ; wait for scanline to finish

	jmp VerticalSYNC  ; start the entire process over!

; ****************************************************************
; This is the 8-bit random number generator subroutine that uses
; the Galois linear feedback shift register (LFSR) approach.
; This generates and returns a number between 0-255.
; Have two of these to avoid dependencies between powerups and
; mutations.
; ****************************************************************

GetRandomPU               ; this is for powerups   

   	lda RandomPU      ; load the accumulator with a seed
    	lsr		  ; logical shift right
    	bcc SkipEor1      ; branch on carry clear
    	eor #$D4          ; exclusive OR (i.e. XOR) function

SkipEor1

   	sta RandomPU      ; store the random number in variable
   	rts               ; return from subroutine

GetRandomMut              ; this is for mutation flag

   	lda RandomMut     ; load the accumulator with a seed
    	lsr		  ; logical shift right
    	bcc SkipEor2      ; branch on carry clear
    	eor #$A9          ; exclusive OR (i.e. XOR) function
                          ; $A9 generates inverse sequence

SkipEor2

   	sta RandomMut     ; store the random number in variable
   	rts               ; return from subroutine

; ****************************************************************
; The graphics for player 0
; ****************************************************************

P0Graphic1	        ; 55 scanlines total
			; extra scanlines help position P0 without
			; keeping track of scanlines. This buys some cycles
			; during the player drawing routine.
			; This is P0 without the tricorder

	.byte %00000000 ; |        | 
    	.byte %10000000 ; |X       | 
    	.byte %10000000 ; |X       |
    	.byte %11000011 ; |XX    XX|
    	.byte %01100010 ; | XX   X |
    	.byte %01100010 ; | XX   X |
    	.byte %00110110 ; |  XX XX |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00011100 ; |   XXX  |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00111100 ; |  XXXX  |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00111010 ; |  XXX X |
    	.byte %00111000 ; |  XXX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00010000 ; |   X    |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
	.byte %11111111 ; |        | <- invisible blue line for missile here
	.byte %00000000 ; |        | for collision detection
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        | 
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        | 42nd scanline here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        | 55th scanline

P0Graphic2	        ; 55 scanlines total
			; This is P0 with a tricorder
			; tricorder has health screen

	.byte %00000000 ; |        | 
    	.byte %10000000 ; |X       |
    	.byte %10000000 ; |X       |
    	.byte %11000011 ; |XX    XX|
    	.byte %01100010 ; | XX   X |
    	.byte %01100010 ; | XX   X |
    	.byte %00110110 ; |  XX XX |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00011100 ; |   XXX  |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00111100 ; |  XXXX  |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00111010 ; |  XXX X |
    	.byte %00111000 ; |  XXX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00010000 ; |   X    |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
	.byte %11111111 ; |        | <- invisible blue line here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %01111110 ; | XXXXXX | tricorder
	.byte %11111111 ; |XXXXXXXX| health screen
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10111101 ; |X XXXX X|
	.byte %10111101 ; |X XXXX X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10000001 ; |X      X|
	.byte %11111111 ; |XXXXXXXX|
	.byte %11000011 ; |XX    XX|
	.byte %01111110 ; | XXXXXX | 42nd scanline here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |

P0Graphic3	        ; 55 scanlines total
			; This is P0 with a tricorder
			; tricorder has hurt screen

	.byte %00000000 ; |        | Doctor
    	.byte %10000000 ; |X       |
    	.byte %10000000 ; |X       |
    	.byte %11000011 ; |XX    XX|
    	.byte %01100010 ; | XX   X |
    	.byte %01100010 ; | XX   X |
    	.byte %00110110 ; |  XX XX |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00011100 ; |   XXX  |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00111100 ; |  XXXX  |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00111010 ; |  XXX X |
    	.byte %00111000 ; |  XXX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00010000 ; |   X    |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
	.byte %11111111 ; |        | <- invisible blue line here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        | 25th scanline here
	.byte %00000000 ; |        |
	.byte %01111110 ; | XXXXXX | tricorder
	.byte %11111111 ; |XXXXXXXX| hurt screen
	.byte %10000001 ; |X      X| exclamation point
	.byte %10011001 ; |X  XX  X|
	.byte %10000001 ; |X      X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10011001 ; |X  XX  X|
	.byte %10000001 ; |X      X|
	.byte %11111111 ; |XXXXXXXX|
	.byte %11000011 ; |XX    XX|
	.byte %01111110 ; | XXXXXX | 42nd scanline here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |

P0Graphic4	        ; 55 scanlines total
			; This is P0 with a tricorder
			; tricorder has neutral screen

	.byte %00000000 ; |        | 
    	.byte %10000000 ; |X       |
    	.byte %10000000 ; |X       |
    	.byte %11000011 ; |XX    XX|
    	.byte %01100010 ; | XX   X |
    	.byte %01100010 ; | XX   X |
    	.byte %00110110 ; |  XX XX |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00011100 ; |   XXX  |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00111100 ; |  XXXX  |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00111010 ; |  XXX X |
    	.byte %00111000 ; |  XXX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00010000 ; |   X    |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
	.byte %11111111 ; |        | <- invisible blue line here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %01111110 ; | XXXXXX | tricorder
	.byte %11111111 ; |XXXXXXXX| neutral screen
	.byte %10000001 ; |X      X| hurt or heal?
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10111101 ; |X XXXX X|
	.byte %10111101 ; |X XXXX X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %11111111 ; |XXXXXXXX|
	.byte %11000011 ; |XX    XX|
	.byte %01111110 ; | XXXXXX | 42nd scanline here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |

P0Graphic5	        ; 55 scanlines total
			; This is P0 with a tricorder
			; tricorder has context-dependent screen

	.byte %00000000 ; |        | 
    	.byte %10000000 ; |X       |
    	.byte %10000000 ; |X       |
    	.byte %11000011 ; |XX    XX|
    	.byte %01100010 ; | XX   X |
    	.byte %01100010 ; | XX   X |
    	.byte %00110110 ; |  XX XX |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00011100 ; |   XXX  |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00111100 ; |  XXXX  |
    	.byte %00111110 ; |  XXXXX |
    	.byte %00111010 ; |  XXX X |
    	.byte %00111000 ; |  XXX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00010000 ; |   X    |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
    	.byte %00011000 ; |   XX   |
	.byte %11111111 ; |        | <- invisible blue line here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %01111110 ; | XXXXXX | tricorder
	.byte %11111111 ; |XXXXXXXX| context-dependent screen
	.byte %10000001 ; |X      X| hurt or heal?
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10100101 ; |X X  X X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10100101 ; |X X  X X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %10000001 ; |X      X|
	.byte %11111111 ; |XXXXXXXX|
	.byte %11000011 ; |XX    XX|
	.byte %01111110 ; | XXXXXX | 42nd scanline here
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |
	.byte %00000000 ; |        |

P0Color	; one color for each of the 55 P0 scanlines   

	.byte $00  
	.byte $02  
	.byte $04  
	.byte $04  
	.byte $04  
	.byte $04  
	.byte $04
	.byte $04
	.byte $02
	.byte $08
	.byte $08
	.byte $0A
	.byte $0A
	.byte $0C
	.byte $0C
	.byte $0E
	.byte $0E
	.byte $02
	.byte $04
	.byte $04
	.byte $02
	.byte $80  ; <- this color matches background
	.byte $00  ; that is used as a marker of the
	.byte $00  ; nucleotide to edit - this makes
	.byte $00  ; missile 0 invisible but open to
	.byte $00  ; collision detection
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A  ; 42nd scanline
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A
	.byte $0A	

; ****************************************************************
; The 8 colors for the patient and dollar score playfields
; ****************************************************************

PScoreColor	       

	.byte $06
	.byte $F0
	.byte $F2
	.byte $F4
	.byte $F6
	.byte $F8
	.byte $FA
	.byte $FC
	.byte $FE
	.byte $30

DScoreColor	       

	.byte $00
	.byte $F0
	.byte $F2
	.byte $F4
	.byte $F6
	.byte $F8
	.byte $FA
	.byte $FC
	.byte $FE
	.byte $06

; ****************************************************************
; The graphics for the score "P" and "$" - 14 scanlines each
; ****************************************************************

PatientGfx

	.byte   #%00111111 ; |XX      |
	.byte   #%00111111 ; |XX      |
        .byte   #%00111111 ; |XX      |
	.byte   #%00111111 ; |XX      |
	.byte   #%00111111 ; |XX      |
	.byte   #%00111111 ; |XX      |
	.byte   #%00111111 ; |XX      |
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%00111100 ; |XX    XX|
        .byte   #%00111100 ; |XX    XX|
	.byte   #%00111100 ; |XX    XX|
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%00000000 ; |XXXXXXXX|

DollarGfx

	.byte   #%11101111 ; |   X    |
	.byte   #%00000000 ; |XXXXXXXX|
        .byte   #%00000000 ; |XXXXXXXX|
	.byte   #%11111100 ; |      XX|
	.byte   #%11111100 ; |      XX|
	.byte   #%11111100 ; |      XX|
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%00111111 ; |XX      |
	.byte   #%00111111 ; |XX      |
        .byte   #%00111111 ; |XX      |
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%00000000 ; |XXXXXXXX|
	.byte   #%11101111 ; |   X    |

; ****************************************************************
; Blank graphics for use in the playfield - 14 scanlines
; ****************************************************************

BlankGfx1

	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
        .byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
        .byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |
	.byte   #%11111111 ; |        |

BlankGfx2

	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
        .byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
        .byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |
	.byte   #%00000000 ; |        |

; ****************************************************************
; The graphics for the "gene medic" name part of start screen
; ****************************************************************
 
Gene1Gfx                   ; graphic for "GE" part of "GENE"

	.byte   #%11101110 ; |XXX XXX |
	.byte   #%11101110 ; |XXX XXX |
        .byte   #%10101000 ; |X X X   |
	.byte   #%10101000 ; |X X X   |
	.byte   #%10101000 ; |X X X   |
	.byte   #%10101000 ; |X X X   |
	.byte   #%11101110 ; |XXX XXX |
	.byte   #%11101110 ; |XXX XXX |
	.byte   #%10001000 ; |X   X   |
	.byte   #%10001000 ; |X   X   |
        .byte   #%10001000 ; |X   X   |
	.byte   #%10001000 ; |X   X   |
	.byte   #%11101110 ; |XXX XXX |
	.byte   #%11101110 ; |XXX XXX |

Gene2Gfx                   ; graphic for "NE" part of "GENE"

	.byte   #%01110101 ; | XXX X X|
	.byte   #%01110101 ; | XXX X X|
        .byte   #%00010101 ; |   X X X|
	.byte   #%00010101 ; |   X X X|
	.byte   #%00010101 ; |   X X X|
	.byte   #%00010101 ; |   X X X|
	.byte   #%01110101 ; | XXX X X|
	.byte   #%01110101 ; | XXX X X|
	.byte   #%00010101 ; |   X X X|
	.byte   #%00010101 ; |   X X X|
        .byte   #%00010101 ; |   X X X|
	.byte   #%00010101 ; |   X X X|
	.byte   #%01110111 ; | XXX XXX|
	.byte   #%01110111 ; | XXX XXX|

Medic1Gfx                  ; graphic for "ME" part of "MEDIC"

	.byte   #%10101110 ; |XX XXX X|
	.byte   #%10101110 ; |XX XXX X|
        .byte   #%10101000 ; |XX X X X|
	.byte   #%10101000 ; |XX X X X|
	.byte   #%10101000 ; | X X X X|
	.byte   #%10101000 ; | X X X X|
	.byte   #%10101110 ; | X X X X|
	.byte   #%10101110 ; | X X X X|
	.byte   #%10101000 ; | X X X X|
	.byte   #%10101000 ; | X X X X|
        .byte   #%11101000 ; | X X X X|
	.byte   #%11101000 ; | X X X X|
	.byte   #%10101110 ; | X XXX X|
	.byte   #%10101110 ; | X XXX X|

Medic2Gfx                   ; graphic for "DIC" part of "MEDIC"

	.byte   #%11010011 ; | X  X XX|
	.byte   #%11010111 ; | X  X XX|
        .byte   #%01010101 ; | X  X X |
	.byte   #%01010101 ; | XX X X |
	.byte   #%01010101 ; | XX X X |
	.byte   #%01010101 ; |  XXX   |
	.byte   #%01010101 ; |   XX   |
	.byte   #%01010101 ; |   XX   |
	.byte   #%01010101 ; |  XXX   |
	.byte   #%01010101 ; | XX X   |
        .byte   #%01010101 ; | XX X   |
	.byte   #%01010101 ; | X  X   |
	.byte   #%11010111 ; | X  X XX|
	.byte   #%11010011 ; | X  X XX|

GeneColor	; one color for each of the 14 scanlines  
                ; for each of the start screen game names
		; The colors for Gene in Gene Medic 

	.byte $02  
	.byte $02  
	.byte $04  
	.byte $04  
	.byte $06  
	.byte $06  
	.byte $08
	.byte $08
	.byte $0A
	.byte $0A
	.byte $0C
	.byte $0C
	.byte $0E
	.byte $0E

MedicColor	; one color for each of the 14 scanlines  
                ; for each of the start screen game names 
		; The colors for Medic in Gene Medic

	.byte $12  
	.byte $12  
	.byte $14  
	.byte $14  
	.byte $16  
	.byte $16  
	.byte $18
	.byte $18
	.byte $1A
	.byte $1A
	.byte $1C
	.byte $1C
	.byte $1E
	.byte $1E

; ****************************************************************
; The graphics for the mitochondria and endoplasmic reticulum
; cytoplasmic elements
; ****************************************************************
 
MitoGfx                    ; graphic for the mitochodria

	.byte   #%01111110 ; | XXXXXX |
	.byte   #%11111111 ; |XXXXXXXX|
        .byte   #%10100001 ; |X X    X|
	.byte   #%10100001 ; |X X    X|
	.byte   #%10100001 ; |X X    X|
	.byte   #%10100001 ; |X X    X|
	.byte   #%10100101 ; |X X  X X|
	.byte   #%10100101 ; |X X  X X|
	.byte   #%10100101 ; |X X  X X|
	.byte   #%10000101 ; |X    X X|
        .byte   #%10000101 ; |X    X X|
	.byte   #%10000101 ; |X    X X|
	.byte   #%11111111 ; |XXXXXXXX|
	.byte   #%01111110 ; | XXXXXX |

ERGfx                      ; graphic for the endoplasmic reticulum

	.byte   #%00000000 ; |        |
	.byte   #%00011000 ; |   XX   |
        .byte   #%11111100 ; |XXXXXX  |
	.byte   #%11111100 ; |XXXXXX  |
	.byte   #%00011000 ; |   XX   |
	.byte   #%00111111 ; |  XXXXXX|
	.byte   #%00111111 ; |  XXXXXX|
	.byte   #%00011000 ; |   XX   |
	.byte   #%01111100 ; | XXXXX  |
	.byte   #%01111100 ; | XXXXX  |
        .byte   #%00110000 ; |  XX    |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%00110000 ; |  XX    |

; ****************************************************************
; The playfield graphics for the DNA double helix A
; Used on alternating screens to simulate movement through cell
; ****************************************************************

HelixA0			; PF0 - only first four bits are read	

	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %10000000	; |X       |
	.byte %01000000	; | X      |
	.byte %00100000	; |  X     |
	.byte %00010000	; |   X    |
	.byte %00100000	; |  X     |
	.byte %01000000	; | X      |
	.byte %10000000	; |X       |
	.byte %00000000	; |        |
	
HelixA1			; PF1

	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %11000001	; |XX     X|
	.byte %00100010	; |  X   X |
	.byte %00010100	; |   X X  |
	.byte %00001000	; |    X   |
	.byte %00010100	; |   X X  |
	.byte %00100010	; |  X   X |
	.byte %11000001	; |XX     X|
	.byte %00000000	; |        |

HelixA2			; PF2

	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %10000011	; |X     XX|
	.byte %01000100	; | X   X  |
	.byte %00101000	; |  X X   |
	.byte %00010000	; |   X    |
	.byte %00101000	; |  X X   |
	.byte %01000100	; | X   X  |
	.byte %10000011	; |X     XX|
	.byte %00000000	; |        |

; ****************************************************************
; The playfield graphics for the DNA double helix B
; Used on alternating screens to simulate movement through cell
; ****************************************************************

HelixB0			; PF0 - only first four bits are read	

	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %10000000	; |X       |
	.byte %01000000	; | X      |
	.byte %00100000	; |  X     |
	.byte %00010000	; |   X    |
	.byte %00100000	; |  X     |
	.byte %01000000	; | X      |
	.byte %10000000	; |X       |
	.byte %00000000	; |        |

HelixB1			; PF1

	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %10000011	; |X     XX|
	.byte %01000100	; | X   X  |
	.byte %00101000	; |  X X   |
	.byte %00010000	; |   X    |
	.byte %00101000	; |  X X   |
	.byte %01000100	; | X   X  |
	.byte %10000011	; |X     XX|
	.byte %00000000	; |        |


HelixB2			; PF2

	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %00000000	; |        |
	.byte %11000001 ; |XX     X|
	.byte %00100010	; |  X   X |
	.byte %00010100	; |   X X  |
	.byte %00001000	; |    X   |
	.byte %00010100	; |   X X  |
	.byte %00100010	; |  X   X |
	.byte %11000001 ; |XX     X|
	.byte %00000000	; |        |

; ****************************************************************
; The graphics for the heart used to indicate win-loss at end 
; ****************************************************************
 
HeartGfx                   ; graphics for the final heart
			   ; only half is needed because it is
			   ; refelected in symmatrical playfield
		           ; need 36 scanlines to draw

	.byte   #%10000000 ; |X       |
	.byte   #%10000000 ; |X       |
	.byte   #%10000000 ; |X       |
	.byte   #%11000000 ; |XX      |
	.byte   #%11000000 ; |XX      |
	.byte   #%11000000 ; |XX      |
	.byte   #%11100000 ; |XXX     |
	.byte   #%11100000 ; |XXX     |
	.byte   #%11100000 ; |XXX     |
        .byte   #%11110000 ; |XXXX    | 10 scanline
        .byte   #%11110000 ; |XXXX    |
	.byte   #%11110000 ; |XXXX    |
	.byte   #%11111000 ; |XXXXX   |
	.byte   #%11111000 ; |XXXXX   |
	.byte   #%11111000 ; |XXXXX   |
	.byte   #%11111100 ; |XXXXXX  |
	.byte   #%11111100 ; |XXXXXX  |
	.byte   #%11111100 ; |XXXXXX  | 
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX | 20 scanline
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111110 ; |XXXXXXX |
	.byte   #%11111100 ; |XXXXXX  |
	.byte   #%11111100 ; |XXXXXX  |
	.byte   #%11111100 ; |XXXXXX  |
	.byte   #%11111100 ; |XXXXXX  | 30 scanline
        .byte   #%11111000 ; |XXXXX   |
        .byte   #%11111000 ; |XXXXX   |
	.byte   #%01110000 ; | XXX    |
	.byte   #%01110000 ; | XXX    |
	.byte   #%00110000 ; |  XX    |
	.byte   #%00110000 ; |  XX    | 36 scanline
	

; ****************************************************************
; The graphics for the money bag and research powerups
; ****************************************************************

MoneyBag                ; Money Bag powerup graphic
			; 16 scanlines

	.byte %00000000	; |        |
	.byte %00111110	; |  XXXXX |
	.byte %01110111	; | XXX XXX|
	.byte %01110111	; | XXX XXX|
	.byte %01100011	; | XX   XX|
	.byte %01111011	; | XXXX XX|
	.byte %01100011	; | XX   XX|
	.byte %01101111	; | XX XXXX|
	.byte %01100011	; | XX   XX|
	.byte %00110110	; |  XX XX |
	.byte %00110110	; |  XX XX |
	.byte %00011100	; |   XXX  |
	.byte %00001000	; |    X   |
 	.byte %00011100	; |   XXX  |
    	.byte %00110110	; |  XX XX |
    	.byte %00000000	; |        |

Research		; Research powerup graphic
			; 16 scanlines	

	.byte %00000000	; |        |
	.byte %11111111	; |XXXXXXXX|
	.byte %10000001	; |X      X|
	.byte %10111101	; |X XXXX X|
	.byte %10000001	; |X      X|
	.byte %10111101	; |X XXXX X|
	.byte %10000001	; |X      X|
	.byte %10111101	; |X XXXX X|
	.byte %10000001	; |X      X|
	.byte %10111101	; |X XXXX X|
	.byte %10000001	; |X      X|
	.byte %10001101	; |X   XX X|
	.byte %10000001	; |X      X|
 	.byte %10100001	; |X X    X|
    	.byte %11111111	; |XXXXXXXX|
    	.byte %00000000	; |        |

EHR			; EHR computer powerup graphic
			; 16 scanlines	

	.byte %00000000	; |        |
	.byte %11111111	; |XXXXXXXX|
	.byte %00000000	; |        |
	.byte %11111111	; |XXXXXXXX|
	.byte %10000001	; |X      X|
	.byte %10111101	; |X XXXX X|
	.byte %10111101	; |X XXXX X|
	.byte %10111101	; |X XXXX X|
	.byte %10111101	; |X XXXX X|
	.byte %10000001	; |X      X|
	.byte %10111001	; |X XXX  X|
	.byte %10111001	; |X XXX  X|
	.byte %10111001	; |X XXX  X|
 	.byte %10000001	; |X      X|
    	.byte %11111111	; |XXXXXXXX|
    	.byte %00000000	; |        |

; ****************************************************************
; The sound effect subroutines and data called from the main loop.
; These came directly from Darrell Spice's tutorial with some
; modifications. See sfx.asm file for more detailed comments.
; See https://is.gd/YDAPkE for information and credit. Thanks!
; ****************************************************************

SFX_F                  ; frequencies 
 
    .byte 0,  3,  3,  3,  2,  2,  2,  1,  1,  1,  0,  0,  0 ; harmful edit
    .byte 0,  0,  0,  0,  1,  1,  1,  2,  2,  2,  3,  3,  3 ; helpful edit
    .byte 0,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8 ; ping
    .byte 0,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7 ; win1
    .byte 0,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8 ; win2
    .byte 0,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9 ; win3
    .byte 0,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20,  20 ; lose1
    .byte 0,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25,  25 ; lose2
    .byte 0,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30,  30 ; lose3
 
SFX_CV                 ; channel and volume

    .byte 0,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f ; harmful edit

sfxHARM = *-SFX_CV-1

    .byte 0,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f,$6f ; helpful edit

sfxHELP = *-SFX_CV-1

    .byte 0,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c ; ping

sfxPING = *-SFX_CV-1

    .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; win1

sfxWIN1 = *-SFX_CV-1

    .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; win2

sfxWIN2 = *-SFX_CV-1

    .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; win3

sfxWIN3 = *-SFX_CV-1

    .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; lose1

sfxLOSE1 = *-SFX_CV-1

    .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; lose2

sfxLOSE2 = *-SFX_CV-1

    .byte 0,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf ; lose3

sfxLOSE3 = *-SFX_CV-1

SFX_OFF
         ldx #0             ; silence sound output
         stx SFX_LEFT       ; reset to 0
         stx SFX_RIGHT      ; reset to 0
         stx AUDV0          ; reset volume to 0
         stx AUDV1	    ; reset volume to 1
         stx AUDC0	    ; reset channel to 0
         stx AUDC1          ; reset channel to 1
         rts                ; return

SFX_TRIGGER
         ldx SFX_LEFT       ; test left channel
         lda SFX_CV,x       ; CV value will be 0 if channel is idle 
         bne .leftnotfree   ; if not 0 then skip ahead
         sty SFX_LEFT       ; channel is idle, use it
         rts                ; all done

.leftnotfree 
         ldx SFX_RIGHT      ; test right channel
         lda SFX_CV,x       ; CV value will be 0 if channel is idle
         bne .rightnotfree  ; if not 0 then skip ahead
         sty SFX_RIGHT      ; channel is idle, use it
         rts                ; all done

.rightnotfree
         cpy SFX_LEFT       ; test sfx priority with left channel
         bcc .leftnotlower  ; skip ahead if new sfx has lower priority than active sfx
         sty SFX_LEFT       ; new sfx has higher priority so use left channel
         rts                ; all done

.leftnotlower
         cpy SFX_RIGHT      ; test sfx with right channel
         bcc .rightnotlower ; skip ahead if new sfx has lower priority than active sfx
         sty SFX_RIGHT      ; new sfx has higher priority so use right channel

.rightnotlower
        rts
 
SFX_UPDATE
         ldx SFX_LEFT       ; get the pointer for the left channel
         lda SFX_F,x        ; get the Frequency value
         sta AUDF0          ; update the Frequency register
         lda SFX_CV,x       ; get the combined Control and Volume value
         sta AUDV0          ; update the Volume register
         lsr                ; prep the Control value,
         lsr                ;   it's stored in the upper nybble
         lsr                ;   but must be in the lower nybble
         lsr                ;   when Control is updated
         sta AUDC0          ; update the Control register
         beq .skipleftdec   ; skip ahead if Control = 0
         dec SFX_LEFT       ; update pointer for left channel

.skipleftdec:
         ldx SFX_RIGHT      ; get the pointer for the right channel
         lda SFX_F,x        ; get the Frequency value
         sta AUDF1          ; update the Frequency register
         lda SFX_CV,x       ; get the combined Control and Volume value
         sta AUDV1          ; update the Volume register
         lsr                ; prep the Control value,
         lsr                ;   it's stored in the upper nybble
         lsr                ;   but must be in the lower nybble
         lsr                ;   when Control is updated
         sta AUDC1          ; update the Control register
         beq .skiprightdec  ; skip ahead if Control = 0
         dec SFX_RIGHT      ; update pointer for right channel

.skiprightdec
         rts                ; all done

; ****************************************************************
; The Interrupts are are used to help rescue the 6502 if it fails
; 2/3 are not used by 6507 chip.
; ****************************************************************

	ORG $FFFA         ; address for the interrupt vectors

InterruptVectors

	.word Reset          ; NMI
	.word Reset          ; RESET
	.word Reset          ; IRQ

END