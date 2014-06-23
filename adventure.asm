      processor 6502                                                                                               
;Default 2600 Constants set up by dissasembler..                                                                   
VSYNC   =  $00                                                                                                     
VBLANK  =  $01                                                                                                     
WSYNC   =  $02                                                                                                     
RSYNC   =  $03                                                                                                     
NUSIZ0  =  $04                                                                                                     
NUSIZ1  =  $05                                                                                                     
COLUP0  =  $06                                                                                                     
COLUP1  =  $07                                                                                                     
COLUPF  =  $08                                                                                                     
COLUBK  =  $09                                                                                                     
CTRLPF  =  $0A                                                                                                     
PF0     =  $0D                                                                                                     
PF1     =  $0E                                                                                                     
PF2     =  $0F                                                                                                     
RESP0   =  $10                                                                                                     
AUDC0   =  $15                                                                                                     
AUDF0   =  $17                                                                                                     
AUDV0   =  $19                                                                                                     
AUDV1   =  $1A                                                                                                     
GRP0    =  $1B                                                                                                     
GRP1    =  $1C                                                                                                     
ENAM0   =  $1D                                                                                                     
ENAM1   =  $1E                                                                                                     
ENABL   =  $1F                                                                                                     
HMP0    =  $20                                                                                                     
VDEL01  =  $26                                                                                                     
HMOVE   =  $2A                                                                                                     
HMCLR   =  $2B                                                                                                     
CXCLR   =  $2C                                                                                                     
CXP0FB  =  $32                                                                                                     
CXP1FB  =  $33                                                                                                     
CXM0FB  =  $34                                                                                                     
CXM1FB  =  $35                                                                                                     
CXBLPF  =  $36                                                                                                     
CXPPMM  =  $37                                                                                                     
INPT4   =  $3C                                                                                                     
SWCHA   =  $0280                                                                                                   
SWCHB   =  $0282                                                                                                   
INTIM   =  $0284                                                                                                   
TIM64T  =  $0296                                                                                                   
                                                                                                                   
       ORG $F000                                                                                                   
                                                                                                                   
START:                                                                                                             
       JMP    StartGame           ;Jump To Start Game                                                        ;3    
                                                                                                                   
                                                                                                                   
;Alternate Start                                                                                                   
      .byte $78,$D8,$4C,$06,$F3  ;Setup for 6507, Start with no Variable Initialisation.                      
                                                                                                                   
;Print Display                                                                                                     
PrintDisplay:                                                                                                             
       STA    HMCLR               ;Clear horzontal motion.                                                   ;3    
       LDA    $86                 ;Position Player00 Sprite To                                               ;3    
       LDX    #$00                ;      the X Coordinate of Object1.                                        ;2    
       JSR    PosSpriteX                                                                                     ;6    
                                                                                                                   
       LDA    $88                 ;Position Player01 Sprite to                                               ;3    
       LDX    #$01                ;      the X Coordinate of Object2.                                        ;2    
       JSR    PosSpriteX                                                                                     ;6    
                                                                                                                   
       LDA    $8B                 ;Position Ball Strite to                                                   ;3    
       LDX    #$04                ;      the X Coordinate of the Man.                                        ;2    
       JSR    PosSpriteX                                                                                     ;6    
                                                                                                                   
       STA    WSYNC               ;Wait for horizontal Blank.                                                ;3    
       STA    HMOVE               ;Apply Horizontal Motion.                                                  ;3    
       STA    CXCLR               ;Clear Collision Latches.                                                  ;3    
                                                                                                                   
       LDA    $8C                 ;Get the Y Coordinate of the Man.                                          ;3    
       SEC                                                                                                   ;2    
       SBC    #$04                ;And Adjust it (By Four Scan Lines)                                        ;2    
       STA    $8D                 ;      for printing (so Y Coordinate Specifies Middle)                     ;3    
                                                                                                                   
PrintDisplay_1: 
	 LDA    INTIM               ;Wait for end of the                                                       ;4    
       BNE    PrintDisplay_1      ;      current fame.                                                       ;2    
                                                                                                                   
       LDA    #$00                                                                                           ;2    
       STA    $90                 ;Set Player00 definition index.                                            ;3    
       STA    $91                 ;Set Player01 definition index.                                            ;3    
       STA    $8F                 ;Set room definition index.                                                ;3    
       STA    GRP1                ;Clear any graphics for Player01.                                          ;3    
       LDA    #$01                                                                                           ;2    
       STA    VDEL01              ;vertically delay Player 01                                                ;3    
       LDA    #$68                                                                                           ;2    
       STA    $8E                 ;Set Scan Lind Count.                                                      ;3    
                                                                                                                   
;Print top line of Room.                                                                                           
       LDY    $8F                 ;Get room definition index.                                                ;3    
       LDA    ($80),Y             ;Get first room definition byte.                                           ;5    
       STA    PF0                 ;      and display.                                                        ;3    
       INY                                                                                                   ;2    
       LDA    ($80),Y             ;Get Next room definition byte.                                            ;5    
       STA    PF1                 ;      and display.                                                        ;3    
       INY                                                                                                   ;2    
       LDA    ($80),Y             ;Get Last room defintion byte.                                             ;5    
       STA    PF2                 ;      and display.                                                        ;3    
       INY                                                                                                   ;2    
       STY    $8F                 ;Save for Next Time.                                                       ;3    
                                                                                                                   
       STA    WSYNC               ;Wait for Horizontal Blank.                                                ;3    
       LDA    #$00                                                                                           ;2    
       STA    VBLANK              ;Clear any Vertical Blank.                                                 ;3    
       JMP    PrintPlayer00                                                                                          ;3    
                                                                                                                   
                                                                                                                   
;Print Player01 (Object 02)                                                                                        
PrintPlayer01:
       LDA    $8E                 ;Get Current Scan Line.                                                    ;3    
       SEC                        ;Have we reached Object2's                                                 ;2    
       SBC    $89                 ;      Y Coordinate?                                                       ;3    
       STA    WSYNC               ;      Wait for Horzonal Blank.                                            ;3    
       BPL    PrintPlayer00       ;If Not, Branch.                                                           ;2    
                                                                                                                   
       LDY    $91                 ;Get the Player01 definition index.                                        ;3    
       LDA    ($84),Y             ;Get the Next Player01 Definition byte                                     ;5    
       STA    GRP1                ;      and display.                                                        ;3    
       BEQ    PrintPlayer00       ;If Zero then Definition finished.                                         ;2    
                                                                                                                   
       INC    $91                 ;Goto next Player01 definition byte.                                       ;5    
                                                                                                                   
;Print Player00 (Object01), Ball (Man) and Room.
PrintPlayer00:                                                                   
	 LDX    #$00                                                                                           ;2    
       LDA    $8E                 ;Get the Current Scan Line.                                                ;3    
       SEC                        ;Have we reached the Object1's                                             ;2    
       SBC    $87                 ;      Y coordinate?                                                       ;3    
       BPL    PrintPlayer00_1     ;If not then Branch.                                                       ;2    
                                                                                                                   
       LDY    $90                 ;Get Player00 definition index.                                            ;3    
       LDA    ($82),Y             ;Get the Next Player00 definition byte.                                    ;5    
       TAX                                                                                                   ;2    
       BEQ    PrintPlayer00_1     ;If Zero then Definition finished.                                         ;2    
                                                                                                                   
       INC    $90                 ;Go to Next Player00 definition byte.                                      ;5    

PrintPlayer00_1:                                                                                                                   
	 LDY    #$00                ;Disable Ball Graphic.                                                     ;2    
       LDA    $8E                 ;Get Scan line count.                                                      ;3    
       SEC                        ;Have we reached the Man's                                                 ;2    
       SBC    $8D                 ;      Y Coordinate?                                                       ;3    
       AND    #$FC                ;Mask value to four either side (getting depth of 8)                       ;2    
       BNE    PrintPlayer00_2     ;If Not, Branch.                                                           ;2    
                                                                                                                   
       LDY    #$02                ;Enable Ball Graphic.                                                      ;2    

PrintPlayer00_2:                                                                                                                   
       LDA    $8E                 ;Get Scan Line Count.                                                      ;3    
       AND    #$0F                ;Have we reached a sixteenth scan line.                                    ;2    
       BNE    PrintPlayer00_4     ;If not, Branch.                                                           ;2    
                                                                                                                   
       STA    WSYNC               ;Wait for Horzontal Blank.                                                 ;3    
       STY    ENABL               ;Enable Ball (If Wanted)                                                   ;3    
       STX    GRP0                ;Display Player 00 definition byte (if wanted)                             ;3    
                                                                                                                   
       LDY    $8F                 ;Get room definition index.                                                ;3    
       LDA    ($80),Y             ;Get first room definition byte,                                           ;5    
       STA    PF0                 ;      and display.                                                        ;3    
       INY                                                                                                   ;2    
       LDA    ($80),Y             ;Get next room definition byte,                                            ;5    
       STA    PF1                 ;      and display.                                                        ;3    
       INY                                                                                                   ;2    
       LDA    ($80),Y             ;Get next room definition byte,                                            ;5    
       STA    PF2                 ;      and display.                                                        ;3    
       INY                                                                                                   ;2    
       STY    $8F                 ;Save for Next Time.                                                       ;3    

PrintPlayer00_3:                                                                                                                   
       DEC    $8E                 ;Goto next scan line.                                                      ;5    
       LDA    $8E                 ;Get the scan line.                                                        ;3    
       CMP    #$08                ;Have we reached to within 8 scanlines of the bottom?                      ;2    
       BPL    PrintPlayer01       ;If not, Branch.                                                           ;2    
                                                                                                                   
       STA    VBLANK              ;Turn on VBLANK                                                            ;3    
       JMP    TidyUp                                                                                        ;3    
                                                                                                                   
;Print Player00 (Object 01) and Ball (Man)  
PrintPlayer00_4:                                                                       
       STA    WSYNC               ;Wait for Horzontal blank.                                                 ;3    
       STY    ENABL               ;Enable Ball (If Wanted.)                                                  ;3    
       STX    GRP0                ;Display Player00 definition byte (if Wanted).                             ;3    
       JMP    PrintPlayer00_3                                                                                          ;3    
                                                                                                                   
;Tidy Up                                                                                                           
TidyUp:
       LDA    #$00                                                                                           ;2    
       STA    GRP1                ;Clear any graphics for Player01                                           ;3    
       STA    GRP0                ;Clear any graphics for Player00                                           ;3    
       LDA    #$20                                                                                           ;2    
       STA    TIM64T              ;Stat Timing this frame using                                              ;4    
       RTS                        ;      the 64 bit counter.                                                 ;6    
                                                                                                                   
                                                                                                                   
;Position Sprite X horizontally.
PosSpriteX:                                                                                   
       LDY    #$02                ;Start with 10 clock cycles (to avoid HBLANK)                              ;2    
       SEC                        ;Divide the Coordinate.                                                    ;2    
PosSpriteX_1:
       INY                        ;      Wanted by Fifteen I.E.                                              ;2    
       SBC    #$0F                ;      Get Course Horizontal                                               ;2    
       BCS    PosSpriteX_1        ;      Value (In Multiples of 5 Clock Cycles                               ;2    
                                  ;      (Therefore giving 15 Color Cycles)                                      
       EOR    #$FF                ;Flip remanter to positive value (inverted).                               ;2    
       SBC    #$06                ;Convert to left or right of current position.                             ;2    
       ASL                                                                                                   ;2    
       ASL                        ;Move to high nybble for TIA                                               ;2    
       ASL                        ;      horizontal motion.                                                  ;2    
       ASL                                                                                                   ;2    
       STY    WSYNC               ;Wait for horozontal blank.                                                ;3    

PosSpriteX_2:                                                                                                                   
       DEY                        ;Count down the color                                                      ;2    
       BPL    PosSpriteX_2        ;      cycles (these are 5 machine/15 color cycles).                       ;2    
                                                                                                                   
       STA    RESP0,X             ;Reset the sprite, thus positioning it coursely.                           ;4    
       STA    HMP0,X              ;Set horizontal (fine) motion of sprite.                                   ;4    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Preform VSYNC  
DoVSYNC:                                                                                                   
       LDA    INTIM               ;Get Timer Output                                                          ;4    
       BNE    DoVSYNC             ;Wait for Time-Out                                                         ;2    
       LDA    #$02                                                                                           ;2    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       STA    VBLANK              ;Start Vertical Blanking.                                                  ;3    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       STA    VSYNC               ;Start verticle sync.                                                      ;3    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       LDA    #$00                                                                                           ;2    
       STA    WSYNC               ;Wait for horizonal blank.                                                 ;3    
       STA    VSYNC               ;End Vertical sync.                                                        ;3    
       LDA    #$2A                ;Set clock interval to                                                     ;2    
       STA    TIM64T              ;Countdown next frame.                                                     ;4    
       RTS                                                                                                   ;6    
                                                                                                                   
;Setup a room for print.                                                                                           
SetupRoomPrint:
       LDA    $8A                 ;Get current room number.                                                  ;3    
       JSR    RoomNumToAddress    ;Convert it to an address.                                                 ;6    
       LDY    #$00                                                                                           ;2    
       LDA    ($93),Y             ;Get low pointer to room                                                   ;5    
       STA    $80                 ;      Graphics                                                            ;3    
       LDY    #$01                                                                                           ;2    
       LDA    ($93),Y             ;Get high pointer to room                                                  ;5    
       STA    $81                 ;      Graphics                                                            ;3    
                                                                                                                   
;Check B&W Switch for foom graphics.                                                                               
       LDA    SWCHB               ;Get console switches.                                                     ;4    
       AND    #$08                ;Check black and white switch                                              ;2    
       BEQ    UseBW               ;Branch if B&W.                                                            ;2    
                                                                                                                   
;Use Color                                                                                                         
       LDY    #$02                                                                                           ;2    
       LDA    ($93),Y             ;Get room color                                                            ;5    
       JSR    ChangeColor         ;Change if necessary                                                       ;6    
       STA    COLUPF              ;Put in Playfiled color register.                                          ;3    
       JMP    UseColor                                                                                          ;3    
                                                                                                                   
;Use B&W                                                                                                           
UseBW:
       LDY    #$03                                                                                           ;2    
       LDA    ($93),Y             ;Get B&W Color                                                             ;5    
       JSR    ChangeColor         ;Change if necessary                                                       ;6    
       STA    COLUPF              ;Put in the Playfield color register.                                      ;3    
                                                                                                                   
;Color Background.
UseColor:                                                                                                 
       LDA    #$08                ;Get light grey background                                                 ;2    
       JSR    ChangeColor         ;Change if necessary                                                       ;6    
       STA    COLUBK              ;Put it in the Background color register.                                  ;3    
                                                                                                                   
;Playfield Control.                                                                                                
       LDY    #$04                                                                                           ;2    
       LDA    ($93),Y             ;Get the playfield control value.                                          ;5    
       STA    CTRLPF              ;And put in the playfield control register.                                ;3    
       AND    #$C0                ;Get the "this wall" flag.                                                 ;2    
       LSR                                                                                                   ;2    
       LSR                                                                                                   ;2    
       LSR                        ;Get the first bit into position.                                          ;2    
       LSR                                                                                                   ;2    
       LSR                                                                                                   ;2    
       STA    ENAM1               ;Enable right hand thin wall. (if wanted - missile01)                      ;3    
       LSR                                                                                                   ;2    
       STA    ENAM0               ;Enable left hand thin wall (if wanted - missle00)                         ;3    
                                                                                                                   
;Get objects to display.                                                                                           
       JSR    CacheObjects        ;Get next two objects to display.                                          ;6    
                                                                                                                   
;Sort out their order.                                                                                             
       LDA    $95                 ;If the object1 is the                                                     ;3    
       CMP    #$00                ;Invisible surround                                                        ;2    
       BEQ    SwapPrintObjects    ;Then branch to swap (we want it as player01)                              ;2    
                                                                                                                   
       CMP    #$5A                ;If the first object is the bridge then                                    ;2    
       BNE    SetupObjectPrint    ;Swap the objects (we want it as player01)                                 ;2    
                                                                                                                   
       LDA    $96                 ;If the object2 is the                                                     ;3    
       CMP    #$00                ;Invisble surround then branch to leave                                    ;2    
       BEQ    SetupObjectPrint    ;it (we want it as player01)                                               ;2    

SwapPrintObjects:                                                                                                                   
       LDA    $95                                                                                            ;3    
       STA    $D8                                                                                            ;3    
       LDA    $96                                                                                            ;3    
       STA    $95                 ;Swap the objects to print.                                                ;3    
       LDA    $D8                                                                                            ;3    
       STA    $96                                                                                            ;3    
                                                                                                                   
;Setup Object1 to print.                                                                                           
SetupObjectPrint:
       LDX    $95                 ;Get Object1                                                               ;3    
       LDA    Store1,X             ;Get low pointer to it's dynamic information.                              ;4    
       STA    $93                                                                                            ;3    
       LDA    Store2,X             ;Get high pointer to it's dynamic informtion.                              ;4    
       STA    $94                                                                                            ;3    
                                                                                                                   
       LDY    #$01                                                                                           ;2    
       LDA    ($93),Y             ;Get Object1's X coordinate                                                ;5    
       STA    $86                 ;and Store for print.                                                      ;3    
       LDY    #$02                                                                                           ;2    
       LDA    ($93),Y             ;Get Object1's Y coordinate                                                ;5    
       STA    $87                 ;and Store for print.                                                      ;3    
                                                                                                                   
       LDA    Store3,X             ;Get low pointer to state value.                                           ;4    
       STA    $93                                                                                            ;3    
       LDA    Store4,X             ;Get high pointer to state value.                                          ;4    
       STA    $94                                                                                            ;3    
       LDY    #$00                                                                                           ;2    
       LDA    ($93),Y             ;Retrieve Object1's current state.                                         ;5    
       STA    $DC                                                                                            ;3    
                                                                                                                   
       LDA    Store5,X             ;Get low pointer to state information.                                     ;4    
       STA    $93                                                                                            ;3    
       LDA    Store6,X             ;Get high pointer to state information.                                    ;4    
       STA    $94                                                                                            ;3    
       JSR    GetObjectState      ;Find current state in the state information.                              ;6    
                                                                                                                   
       INY                        ;Index to the state's corresponding graphic pointer.                       ;2    
       LDA    ($93),Y             ;Get Object1's low graphic address                                         ;5    
       STA    $82                 ;and store for print.                                                      ;3    
       INY                                                                                                   ;2    
       LDA    ($93),Y             ;Get Object1's high graphic address                                        ;5    
       STA    $83                 ;and store for print.                                                      ;3    
                                                                                                                   
;Check B&W for object01                                                                                            
       LDA    SWCHB               ;Get console switches                                                      ;4    
       AND    #$08                ;Check B&W switches.                                                       ;2    
       BEQ    MakeObjectBW        ;Branch if B&W.                                                            ;2    
                                                                                                                   
;Colour                                                                                                            
       LDA    Store7,X             ;Get Object1's Color.                                                      ;4    
       JSR    ChangeColor         ;Change if necessary.                                                      ;6    
       STA    COLUP0              ;And set color luminance00.                                                ;3    
       JMP    ResizeObject                                                                                          ;3    
                                                                                                                   
;B&W
MakeObjectBW:                                                                                                               
       LDA    Store8,X             ;Get Object's B&W Color.                                                   ;4    
       JSR    ChangeColor         ;Change if necessary.                                                      ;6    
       STA    COLUP0              ;Set color luminance00.                                                    ;3    
                                                                                                                   
;Object1 Size 
ResizeObject:                                                                                                     
       LDA    Store9,X             ;Get Object1's Size                                                        ;4    
       ORA    #$10                ;And set to larger size if necessary.                                      ;2    
       STA    NUSIZ0              ;(Used by bridge and invisible surround)                                   ;3    
                                                                                                                   
;Setup Object 2 to Print.                                                                                          
       LDX    $96                 ;Get Object 2                                                              ;3    
       LDA    Store1,X                                                                                        ;4    
       STA    $93                 ;Get low pointer to it's dynamic information.                              ;3    
       LDA    Store2,X                                                                                        ;4    
       STA    $94                 ;Get high pointer to it's dynamic information.                             ;3    
       LDY    #$01                                                                                           ;2    
       LDA    ($93),Y             ;Get Object2's X coordinate                                                ;5    
       STA    $88                 ;and store for print.                                                      ;3    
       LDY    #$02                                                                                           ;2    
       LDA    ($93),Y             ;Get Object2's Y coordinate                                                ;5    
       STA    $89                 ;and store for print.                                                      ;3    
       LDA    Store3,X             ;Get low pointer to state value.                                           ;4    
       STA    $93                                                                                            ;3    
       LDA    Store4,X             ;Get high pointer to state value.                                          ;4    
       STA    $94                                                                                            ;3    
                                                                                                                   
                                                                                                                   
       LDY    #$00                                                                                           ;2    
       LDA    ($93),Y             ;Retrieve Object2's current state.                                         ;5    
       STA    $DC                                                                                            ;3    
       LDA    Store5,X             ;Get low pointer to state information.                                     ;4    
       STA    $93                                                                                            ;3    
       LDA    Store6,X             ;Get high pointer to state information.                                    ;4    
       STA    $94                                                                                            ;3    
       JSR    GetObjectState      ;Find the current state in the state information.                          ;6    
                                                                                                                   
       INY                        ;Index to the state's corresponding graphic pointer.                       ;2    
       LDA    ($93),Y                                                                                        ;5    
       STA    $84                 ;Get Object2's low graphic address.                                        ;3    
       INY                                                                                                   ;2    
       LDA    ($93),Y             ;Get Object2's high graphic address.                                       ;5    
       STA    $85                                                                                            ;3    
                                                                                                                   
;Check B&W for Object2                                                                                             
       LDA    SWCHB               ;Get Console Switches                                                      ;4    
       AND    #$08                ;Check B&W Switch.                                                         ;2    
       BEQ    MakeObject2BW       ;If B&W then Branch.                                                       ;2    
                                                                                                                   
;Color                                                                                                             
       LDA    Store7,X             ;Get Object2;s Color                                                       ;4    
       JSR    ChangeColor         ;Change if Necessary.                                                      ;6    
       STA    COLUP1              ;and set color luminance01.                                                ;3    
       JMP    ResizeObject2                                                                                  ;3    
                                                                                                                   
;B&W 
MakeObject2BW:                                                                                                              
       LDA    Store8,X             ;Get Object's B&W Color.                                                   ;4    
       JSR    ChangeColor         ;Change if Necessary.                                                      ;6    
       STA    COLUP1              ;and set color luminance01.                                                ;3    
                                                                                                                   
;Object2 Size                                                                                                      
ResizeObject2:
       LDA    Store9,X             ;Get Object2's Size                                                        ;4    
       ORA    #$10                ;And set to larger size if necessary.                                      ;2    
       STA    NUSIZ1              ;(Used by bridge and invisible surround)                                   ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
;Fill cache with two objects in this room.                                                                         
CacheObjects:
       LDY    $9C                 ;Get Last Object                                                           ;3    
       LDA    #$A2                ;Set cache to                                                              ;2    
       STA    $95                 ;      no-ojects.                                                          ;3    
       STA    $96                                                                                            ;3    
MoveNextObject:
       TYA                                                                                                   ;2    
       CLC                        ;Goto the next object to                                                   ;2    
       ADC    #$09                ;check (add nine).                                                         ;2    
       CMP    #$A2                ;Check if over maximum.                                                    ;2    
       BCC    GetObjectsInfo                                                                                          ;2    
       LDA    #$00                ;If so, wrap to zero.                                                      ;2    
GetObjectsInfo:
       TAY                                                                                                   ;2    
       LDA    Store1,Y             ;Get low byte of object info.                                              ;4    
       STA    $93                                                                                            ;3    
       LDA    Store2,Y             ;Get high byte of object info.                                             ;4    
       STA    $94                                                                                            ;3    
       LDX    #$00                                                                                           ;2    
       LDA    ($93,X)             ;Get objects current room.                                                 ;6    
       CMP    $8A                 ;Is it in this room?                                                       ;3    
       BNE    CheckForMoreObjects ;If not lets try next object (branch)                                      ;2    
                                                                                                                   
       LDA    $95                 ;Check first slot.                                                         ;3    
       CMP    #$A2                ;If not default (no-object)                                                ;2    
       BNE    StoreObjectToPrint  ;then branch.                                                              ;2    
                                                                                                                   
       STY    $95                 ;Store this object's number to print                                       ;3    
       JMP    CheckForMoreObjects ;      and try for more.                                                   ;3    

StoreObjectToPrint:                                                                                                                   
       STY    $96                 ;Store this object's number to print.                                      ;3    
       JMP    StoreCount          ;      and then give up - no slots free.                                   ;3    
                                                                                                                   
CheckForMoreObjects:
       CPY    $9C                 ;Have we done all the objets?                                              ;3    
       BNE    MoveNextObject      ;If not, continue.                                                         ;2    

StoreCount:                                                                                                                   
       STY    $9C                 ;If so, store current count                                                ;3    
       RTS                        ;      for next time.                                                      ;6    
                                                                                                                   
;Convert room number to address.  
RoomNumToAddress:                                                                                 
       STA    $D8                 ;Strore room number wanted.                                                ;3    
       STA    $93                                                                                            ;3    
       LDA    #$00                ;Zero the high byte of the                                                 ;2    
       STA    $94                 ;      offset.                                                             ;3    
       CLC                                                                                                   ;2    
       ROL    $93                                                                                            ;5    
       ROL    $94                 ;Multiply room number by eight.                                            ;5    
       ROL    $93                                                                                            ;5    
       ROL    $94                                                                                            ;5    
       ROL    $93                                                                                            ;5    
       ROL    $94                                                                                            ;5    
                                                                                                                   
       LDA    $D8                 ;Get the original room number.                                             ;3    
       CLC                                                                                                   ;2    
       ADC    $93                                                                                            ;3    
       STA    $93                 ;And add it to the offset.                                                 ;3    
       LDA    #$00                                                                                           ;2    
       ADC    $94                 ;In effect the room number is                                              ;3    
       STA    $94                 ;      multiplied by nine.                                                 ;3    
                                                                                                            
       LDA    #<RoomDataTable                                                                                ;2    
       CLC                                                                                                   ;2    
       ADC    $93                 ;Add the room data base address                                            ;3    
       STA    $93                 ;to the offset therefore getting                                           ;3    
       LDA    #>RoomDataTable     ;      the final room data address.                                        ;2    
       ADC    $94                                                                                            ;3    
       STA    $94                                                                                            ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
;Get pointer to current state.
GetObjectState:                                                                                     
       LDY    #$00                                                                                           ;2    
       LDA    $DC                 ;Get the current object state.                                             ;3    
GetObjectState_1:
       CMP    ($93),Y             ;Have we found it in the list of states.                                   ;5    
       BCC    GetObjectState_2    ;If nearing it then found it and return                                    ;2    
       BEQ    GetObjectState_2    ;If found it then return.                                                  ;2    
                                                                                                                   
       INY                                                                                                   ;2    
       INY                        ;Goto next state in list of states.                                        ;2    
       INY                                                                                                   ;2    
       JMP    GetObjectState_1                                                                               ;3    
GetObjectState_2:
       RTS                                                                                                   ;6    
                                                                                                                   
;Check for input.
CheckInput:                                                                                                  
	 INC    $E5                 ;Increment low count.                                                      ;5    
       BNE    GetJoystick                                                                                         ;2    
                                                                                                                   
       INC    $E6                 ;Increment hight count if                                                  ;5    
       BNE    GetJoystick         ;      needed.                                                             ;2    
                                                                                                                   
       LDA    #$80                ;Wrap the high count (indicating                                           ;2    
       STA    $E6                 ;      timeout) if needed.                                                 ;3    

GetJoystick:                                                                                                                   
	 LDA    SWCHA               ;Get joystick values.                                                      ;4    
       CMP    #$FF                ;If any movement then branch.                                              ;2    
       BNE    GetJoystick_2                                                                                          ;2    
                                                                                                                   
       LDA    SWCHB               ;Get the consol switches                                                   ;4    
       AND    #$03                ;Mast for the reset/select switchs.                                        ;2    
       CMP    #$03                ;Have either of them been used?                                            ;2    
       BEQ    GetJoystick_3       ;If not branch.                                                            ;2    

GetJoystick_2:                                                                                                                   
	 LDA    #$00                ;Zero the high count of the                                                ;2    
       STA    $E6                 ;      switches or joystick have been used.                                ;3    
GetJoystick_3:
	 RTS                                                                                                   ;6    
                                                                                                                   
;Change color if necessary.                                                                                        
ChangeColor:
	 LSR                        ;If bit 0 of the color is set                                              ;2    
       BCC    ChangeColor_2       ;      then the room is to flash.                                          ;2    
                                                                                                                   
       TAY                        ;Use color as an index (usually E5- the low counter).                      ;2    
       LDA.wy $0080,Y             ;Get flash color (usually the low counter.)                                ;4    

ChangeColor_2:                                                                                                                   
       LDY    $E6                 ;Get the input counter.                                                    ;3    
       BPL    ChangeColor_3       ;If console/joystick moved reciently then branch.                          ;2    
                                                                                                                   
       EOR    $E6                 ;Merge the high counter with the color wanted.                             ;3    
       AND    #$FB                ;Keep this color bug merge down the luminance.                             ;2    

ChangeColor_3:                                                                                                                   
       ASL                        ;And restore original color if necessary.                                  ;2    
       RTS                                                                                                   ;6    
                                                                                                                   
;Get the address of the dynamic information for an object.                                                         
GetObjectAddress:
       LDA    Store1,X                                                                                        ;4    
       STA    $93                 ;Get and store the low address.                                            ;3    
       LDA    Store2,X                                                                                        ;4    
       STA    $94                 ;Get and store the high address.                                           ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
;Game Start
StartGame:                                                                                                        
	 SEI                        ;Set Interupts Off                                                         ;2    
       CLD                                                                                                   ;2    
       LDX    #$28                ;Clear TIA Registers                                                       ;2    
       LDA    #$00                ;&04-&2C i.e. blank                                                        ;2    
ResetAll:
       STA    NUSIZ0,X            ;Everything And Turn.                                                      ;4    
       DEX                        ;Everything Off.                                                           ;2    
       BPL    ResetAll                                                                                          ;2    
       TXS                        ;Reset Stack                                                               ;2    
SetupVars:
       STA    VSYNC,X             ;Clear &80 to &FF User Vars.                                               ;4    
       DEX                                                                                                   ;2    
       BMI    SetupVars                                                                                          ;2    
       JSR    ThinWalls           ;Position the thin walls (missiles)                                        ;6    
       JSR    SetupRoomObjects    ;Setup objects rooms and positions.                                        ;6    
MainGameLoop:
       JSR    CheckGameStart      ;Check for Game Start                                                      ;6    
       JSR    MakeSound           ;Make noise if necessary                                                   ;6    
       JSR    CheckInput          ;Check for input.                                                          ;6    
       LDA    $DE                 ;Is The Game Active?                                                       ;3    
       BNE    NonActiveLoop       ;If Not Branch..                                                           ;2    
       LDA    $B9                 ;Get the room the Chalise is in.                                           ;3    
       CMP    #$12                ;Is it in the yellow castle?                                               ;2    
       BNE    MainGameLoop_2      ;If Not Branch..                                                           ;2    
       LDA    #$FF                                                                                           ;2    
       STA    $DF                 ;Set the note count to maximum.                                            ;3    
       STA    $DE                 ;Set the game to inactive.                                                 ;3    
       LDA    #$00                ;Set the noise type to end-noise.                                          ;2    
       STA    $E0                                                                                            ;3    
MainGameLoop_2:
	 LDY    #$00                ;Allow joystick read - all movement.                                       ;2    
       JSR    BallMovement        ;Check ball collisions and move ball.                                      ;6    
       JSR    MoveCarriedObject   ;Move the Carried Object                                                   ;6    
       JSR    DoVSYNC             ;Wait for VSYNC                                                            ;6    
       JSR    SetupRoomPrint      ;Setup the room and objects for display.                                   ;6    
       JSR    PrintDisplay        ;Display the room and objects.                                             ;6    
       JSR    PickupPutdown       ;Deal with object pickup and putdown.                                      ;6    
       LDY    #$01                ;Dissalow joystick read - move vertically only.                            ;2    
       JSR    BallMovement        ;Check ball collisions and move ball.                                      ;6    
       JSR    Surround            ;Deal With Invisible Surround Moving.                                      ;6    
       JSR    DoVSYNC             ;Wait for VSYNC                                                            ;6    
       JSR    MoveBat             ;Move and deal with bat.                                                   ;6    
       JSR    Portals             ;Move and deal with portcullises.                                          ;6    
       JSR    PrintDisplay        ;Display the room and objects.                                             ;6    
       JSR    MoveGreenDragon     ;Move and deal with the green dragon.                                      ;6    
       JSR    MoveYellowDragon    ;Move and deal with the yellow dragon.                                     ;6    
       JSR    DoVSYNC             ;Wait for VSYNC.                                                           ;6    
       LDY    #$02                ;Dissalow joystic read/bridge check - move horrizonally only.              ;2    
       JSR    BallMovement        ;Check ball collisions and move ball.                                      ;6    
       JSR    MoveRedDragon       ;Move and deal with red dragon.                                            ;6    
       JSR    Mag_1               ;Deal with the magnet.                                                     ;6    
       JSR    PrintDisplay        ;Display the room and objects.                                             ;6    
       JMP    MainGameLoop                                                                                 ;3    
                                                                                                                   
;Non Active Game Loop.                                                                                             
NonActiveLoop:
	 JSR    DoVSYNC             ;Wait for VSYNC                                                            ;6    
       JSR    PrintDisplay        ;Display the room and objects.                                             ;6    
       JSR    SetupRoomPrint      ;Set up room and objects for display.                                      ;6    
       JMP    MainGameLoop                                                                                 ;3    
                                                                                                                   
;Position missiles to "thin wall" areas.
ThinWalls:                                                                           
       LDA    #$0D                ;Position missile 00 to                                                    ;2    
       LDX    #$02                ;(0D,00) - left thin wall.                                                 ;2    
       JSR    PosSpriteX                                                                                     ;6    
       LDA    #$96                ;Position missile 01 to                                                    ;2    
       LDX    #$03                ;(96,00) - right thin wall.                                                ;2    
       JSR    PosSpriteX                                                                                     ;6    
       STA    WSYNC                     ;Wait for horizonal blank.                                           ;3    
       STA    HMOVE                     ;Apply the horizonal move.                                           ;3    
       RTS                                                                                                   ;6    

CheckGameStart:                                                                                                                   
       LDA    SWCHB               ;Get the console switches.                                                 ;4    
       EOR    #$FF                ;Flip (as reset active low).                                               ;2    
       AND    $92                       ;Compare with what was before                                        ;3    
       AND    #$01                      ;And check only the reset switch                                     ;2    
       BEQ    CheckReset          ;If no reset then branch.                                                  ;2    
       LDA    $DE                 ;Has the Game Started?                                                     ;3    
       CMP    #$FF                ;If not then branch.                                                       ;2    
       BEQ    SetupRoomObjects                                                                               ;2    
       LDA    #$11                ;Get the yellow castle room.                                               ;2    
       STA    $8A                       ;Make it the current room.                                           ;3    
       STA    $E2                       ;Make it the previous room.                                          ;3    
       LDA    #$50                ;Get the X coordinate.                                                     ;2    
       STA    $8B                       ;Make it the current ball X coordinate.                              ;3    
       STA    $E3                       ;Make it the previous ball X coordinate.                             ;3    
       LDA    #$20                ;Get the Y coordinate.                                                     ;2    
       STA    $8C                       ;Make it the current ball Y coordinate.                              ;3    
       STA    $E4                       ;Make it the previous ball Y coordinate.                             ;3    
       LDA    #$00                                                                                           ;2    
       STA    $A8                 ;Set the red dragon's state to OK.                                         ;3    
       STA    $AD                 ;Set the yellow dragon's state to OK.                                      ;3    
       STA    $B2                 ;Set the green dragon's state to OK.                                       ;3    
       STA    $DF                 ;Set the note count to zero.. (ops!??)                                     ;3    
       LDA    #$A2                                                                                           ;2    
       STA    $9D                 ;Set no object being carried.                                              ;3    
CheckReset:
       LDA    SWCHB               ;Get the console switches.                                                 ;4    
       EOR    #$FF                ;Flip (as select active low)                                               ;2    
       AND    $92                 ;Compare with what was before.                                             ;3    
       AND    #$02                ;And check only the select switch.                                         ;2    
       BEQ    StoreSwitches       ;Branch if select not being used.                                          ;2    
       LDA    $8A                 ;Get the Current Room.                                                     ;3    
       CMP    #$00                ;Is it the "Number" room?                                                  ;2    
       BNE    SetupRoomObjects    ;Branch if not.                                                            ;2    
       LDA    $DD                 ;Increment the level.                                                      ;3    
       CLC                        ;Number (by two).                                                          ;2    
       ADC    #$02                                                                                           ;2    
       CMP    #$06                ;Have we reached the maximum?                                              ;2    
       BCC    ResetSetup                                                                                     ;2    
       LDA    #$00                ;If yep then set back to zero.                                             ;2    
ResetSetup:
       STA    $DD                 ;Store the new level number.                                               ;3    
SetupRoomObjects:
       LDA    #$00                ;Set the current room to the                                               ;2    
       STA    $8A                 ;"Number" room.                                                            ;3    
       STA    $E2                 ;And the previous room.                                                    ;3    
       LDA    #$00                ;Set the ball's Y coordinate to zero.                                      ;2    
       STA    $8C                 ;And the previous Y coordinate.                                            ;3    
       STA    $E4                 ;(So can't be seen.)                                                       ;3    
       LDY    $DD                 ;Get the level number.                                                     ;3    
       LDA    Loc_4,Y              ;Get the low pointer to object locations.                                  ;4    
       STA    $93                                                                                            ;3    
       LDA    Loc_5,Y             ;Get the high pointer to object locations.                                 ;4    
       STA    $94                                                                                            ;3    
       LDY    #$30                ;Copy all the objects dynamic information.                                 ;2    
SetupRoomObjects_2:
       LDA    ($93),Y             ;(the rooms and positions) into                                            ;5    
       STA.wy $00A1,Y             ;the working area.                                                         ;5    
       DEY                                                                                                   ;2    
       BPL    SetupRoomObjects_2                                                                                          ;2    
       LDA    $DD                 ;Get the level number.                                                     ;3    
       CMP    #$04                ;Branch if level one.                                                      ;2    
       BCC    SignalGameStart     ;Or two (Where all objects are in defined areas.)                          ;2    
       JSR    RandomizeLevel3     ;Put some objects in random rooms.                                         ;6    
       JSR    DoVSYNC             ;Wait for VSYNC                                                            ;6    
       JSR    PrintDisplay        ;Display rooms and objects.                                                ;6    
SignalGameStart:
       LDA    #$00                ;Signal that the game has started.                                         ;2    
       STA    $DE                                                                                            ;3    
       LDA    #$A2                ;Set no object being carried.                                              ;2    
       STA    $9D                                                                                            ;3    
StoreSwitches:
       LDA    SWCHB               ;Store the current console switches                                        ;4    
       STA    $92                                                                                            ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
;Put objects in random rooms for level 3.                                                                          
RandomizeLevel3:
       LDY    #$1E                ;For each of the eleven objects..                                          ;2    
RandomizeLevel3_2:
       LDA    $E5                 ;Get the low input counter as seed.                                        ;3    
       LSR                                                                                                   ;2    
       LSR                                                                                                   ;2    
       LSR                        ;Generate a psudo-random                                                   ;2    
       LSR                        ;room number.                                                              ;2    
       LSR                                                                                                   ;2    
       SEC                                                                                                   ;2    
       ADC    $E5                 ;Store the low input counter.                                              ;3    
       STA    $E5                                                                                            ;3    
       AND    #$1F                ;Trim so represents a room value.                                          ;2    
       CMP    Loc_2,Y             ;If it is less than the                                                    ;4    
       BCC    RandomizeLevel3_2   ;lower bound for object then get another.                                  ;2    
       CMP    Loc_3,Y             ;If it equals or is                                                        ;4    
       BEQ    RandomizeLevel3_3   ;Less than the higher bound for object                                     ;2    
       BCS    RandomizeLevel3_2   ;Then continue (branch if higher)                                          ;2    
RandomizeLevel3_3:
       LDX    Loc_1,Y             ;Get the dynamic data index for this object                                ;4    
       STA    VSYNC,X             ;Store the new room value.                                                 ;4    
       DEY                                                                                                   ;2    
       DEY                        ;Goto the next object.                                                     ;2    
       DEY                                                                                                   ;2    
       BPL    RandomizeLevel3_2   ;Untill all done                                                           ;2    
       RTS                                                                                                   ;6    
                                                                                                                   
;Room Bounds Data.                                                                                                 
;Ex. the chalise at location &B9 can only exist in rooms 13-1A for                                                 
;     level 3.                                                                                                     
Loc_1:                                                                                                                   
       .byte $B9                    ;                                                                          
Loc_2:
       .byte $13                    ;Chalise                                                                   
Loc_3:
       .byte $1A                    ;                                                                          
       .byte $A4,$01,$1D            ;Red Dragon                                                                    
       .byte $A9,$01,$1D            ;Yellow Dragon                                                                 
       .byte $AE,$01,$1D            ;Green Dragon                                                                  
       .byte $B6,$01,$1D            ;Sword                                                                         
       .byte $BC,$01,$1D            ;Bridge                                                                        
       .byte $BF,$01,$1D            ;Yellow Key                                                                    
       .byte $C2,$01,$16            ;White Key                                                                     
       .byte $C5,$01,$12            ;Black Key                                                                     
       .byte $CB,$01,$1D            ;Bat                                                                           
       .byte $B3,$01,$1D            ;Magnet                                                                        

Loc_4:                                                                                                                   
       .byte <Game1Objects                    ;Pointer to object locations for game 01.                                  
Loc_5:
       .byte >Game1Objects                    ;      --continued.                                                        
       .byte <Game2Objects,>Game2Objects                ;Pointer to object locations for game 02.                                    
       .byte <Game2Objects,>Game2Objects                ;Pointer to object locations for game 03.                                    
                                                                                                                   
;Object locations (room and coordinate) for game 01.                                                               
Game1Objects:
       .byte $15,$51,$12            ;Black dot (Room, X, Y)                                                        
       .byte $0E,$50,$20,$00,$00    ;Red Dragon (Room, X, Y, Movement, State)                                    
       .byte $01,$50,$20,$00,$00    ;Yellow Dragon (Room, X, Y, Movement, State)                                 
       .byte $1D,$50,$20,$00,$00    ;Green Dragon (Room, X, Y, Movement, State)                                  
       .byte $1B,$80,$20            ;Magnet (Room,X,Y)                                                             
       .byte $12,$20,$20            ;Sword (Room,X,Y)                                                              
       .byte $1C,$30,$20            ;Challise (Room,X,Y)                                                           
       .byte $04,$29,$37            ;Bridge (Room,X,Y)                                                             
       .byte $11,$20,$40            ;Yellow Key (Room,X,Y)                                                         
       .byte $0E,$20,$40            ;White Key (Room,X,Y)                                                          
       .byte $1D,$20,$40            ;Black Key (Room,X,Y)                                                          
       .byte $1C                    ;Portcullis State                                                          
       .byte $1C                    ;Portcullis State                                                          
       .byte $1C                    ;Portcullis State                                                          
       .byte $1A,$20,$20,$00,$00    ;Bat (Room, X, Y, Movement, State)                                          
       .byte $78,$00                ;Bat (Carrying, Fed-Up)                                                      
                                                                                                                   
;Object locations (room and coordinate) for Games 02 and 03.                                                       
Game2Objects:
       .byte $15,$51,$12            ;Black Dot (Room,X,Y)                                                          
       .byte $14,$50,$20,$A0,$00    ;Red Dragon (Room,X,Y,Movement,State)                                        
       .byte $19,$50,$20,$A0,$00    ;Yellow Dragon (Room,X,Y,Movement,State)                                     
       .byte $04,$50,$20,$A0,$00    ;Green Dragon (Room,X,Y,Movement,State)                                      
       .byte $0E,$80,$20            ;Magnet (Room,X,Y)                                                             
       .byte $11,$20,$20            ;Sword (Room,X,Y)                                                              
       .byte $14,$30,$20            ;Chalise (Room,X,Y)                                                            
       .byte $0B,$40,$40            ;Bridge (Room,X,Y)                                                             
       .byte $09,$20,$40            ;Yellow Key (Room,X,Y)                                                         
       .byte $06,$20,$40            ;White Key (Room,X,Y)                                                          
       .byte $19,$20,$40            ;Black Key (Room,X,Y)                                                          
       .byte $1C                    ;Portcullis State                                                          
       .byte $1C                    ;Portcullis State                                                          
       .byte $1C                    ;Portcullis State                                                          
       .byte $02,$20,$20,$90,$00    ;Bat (Room,X,Y,Movement,State)                                               
       .byte $78,$00                ;Bat (Carrying, Fed-Up)                                                      
                                                                                                                   
;Check ball collisions and move ball.                                                                              
BallMovement:
       LDA    CXBLPF                                                                                         ;3    
       AND    #$80                ;Get ball-playfield collision                                              ;2    
       BNE    PlayerCollision     ;Branch if collision (Player-Wall)                                         ;2    
                                                                                                                   
       LDA    CXM0FB                                                                                         ;3    
       AND    #$40                ;Get ball-missile00 collision.                                             ;2    
       BNE    PlayerCollision     ;Branch if collision. (Player-Left Thin)                                   ;2    
                                                                                                                   
       LDA    CXM1FB                                                                                         ;3    
       AND    #$40                ;Get ball-missile01 collision.                                             ;2    
       BEQ    BallMovement_2      ;Branch if no collision.                                                   ;2    
                                                                                                                   
       LDA    $96                 ;If object2 (to print) is                                                  ;3    
       CMP    #$87                ;      not the black dot then collide.                                     ;2    
       BNE    PlayerCollision                                                                                ;2    
BallMovement_2:                                                                                                                   
       LDA    CXP0FB                                                                                         ;3    
       AND    #$40                ;Get ball-player00 collision.                                              ;2    
       BEQ    BallMovement_3      ;If no collision then branch.                                              ;2    
                                                                                                                   
       LDA    $95                 ;If object1 (to print) is                                                  ;3    
       CMP    #$00                ;      not the invisible surround then                                     ;2    
       BNE    PlayerCollision     ;      branch (collision)                                                  ;2    

BallMovement_3:                                                                                                                   
       LDA    CXP1FB                                                                                         ;3    
       AND    #$40                ;Get ball-player01 collision.                                              ;2    
       BEQ    NoCollision         ;If no collision then branch.                                              ;2    
                                                                                                                   
       LDA    $96                 ;If player01 to print is                                                   ;3    
       CMP    #$00                ;      not the invisible surround then                                     ;2    
       BNE    PlayerCollision     ;      branch (collision)                                                  ;2    
                                                                                                                   
       JMP    NoCollision         ;No collision - branch.                                                    ;3    
                                                                                                                   
;Player collided (with something)                                                                                  
PlayerCollision:
       CPY    #$02                ;Are we checking for the bridge?                                           ;2    
       BNE    ReadStick           ;If not, branch.                                                           ;2    
                                                                                                                   
       LDA    $9D                 ;Get the object being carried.                                             ;3    
       CMP    #$5A                ;      Branch if it is the bridge.                                         ;2    
       BEQ    ReadStick                                                                                      ;2    
                                                                                                                   
       LDA    $8A                 ;Get the current room.                                                     ;3    
       CMP    $BC                 ;Is the bridge in this room.                                               ;3    
       BNE    ReadStick           ;If not branch.                                                            ;2    
                                                                                                                   
;Check going through the bridge.                                                                                   
       LDA    $8B                 ;Get the ball's X coordinate.                                              ;3    
       SEC                                                                                                   ;2    
       SBC    $BD                 ;Subtract the bridge's X coordinate.                                       ;3    
       CMP    #$0A                ;If less than &0A then forget it.                                          ;2    
       BCC    ReadStick                                                                                      ;2    
                                                                                                                   
       CMP    #$17                ;If more than &17 then forget it.                                          ;2    
       BCS    ReadStick                                                                                      ;2    
                                                                                                                   
       LDA    $BE                 ;Get the bridge's Y coordinate.                                            ;3    
       SEC                                                                                                   ;2    
       SBC    $8C                 ;Subtrac the ball's Y coordinate.                                          ;3    
       CMP    #$FC                                                                                           ;2    
       BCS    NoCollision         ;If more than &FC then going through bridge.                               ;2    
                                                                                                                   
       CMP    #$19                ;If more than &19 then forget it.                                          ;2    
       BCS    ReadStick                                                                                      ;2    
                                                                                                                   
;No collision (and going through bridge)                                                                           
NoCollision:
       LDA    #$FF                ;Reset the joystick input.                                                 ;2    
       STA    $99                                                                                            ;3    
       LDA    $8A                 ;Get the current room.                                                     ;3    
       STA    $E2                 ;      and store temporarily.                                              ;3    
       LDA    $8B                 ;Get the ball's X coordinate.                                              ;3    
       STA    $E3                 ;      and store temporarily.                                              ;3    
       LDA    $8C                 ;Get the ball's Y coordinate.                                              ;3    
       STA    $E4                 ;And Store Temporarily.                                                    ;3    
                                                                                                                   
;Read Sticks                                                                                                            
ReadStick:
       CPY    #$00                ;???Is game in first phase?                                                ;2    
       BNE    ReadStick_2         ;If not, don't bother with joystick read.                                  ;2    
                                                                                                                   
       LDA    SWCHA               ;Read joysticks.                                                           ;4    
       STA    $99                 ;      and store value.                                                    ;3    

ReadStick_2:                                                                                                                   
       LDA    $E2                 ;Get Temporary room.                                                       ;3    
       STA    $8A                 ;      and make it the current room.                                       ;3    
       LDA    $E3                 ;Get temporary X coordinate                                                ;3    
       STA    $8B                 ;      and make it the man's X coordinate.                                 ;3    
       LDA    $E4                 ;Get temporary Y coordinate                                                ;3    
       STA    $8C                 ;      and make it the man's Y coordinate.                                 ;3    
                                                                                                                   
       LDA    $99                 ;Get the Joystick position.                                                ;3    
       ORA    ReadStick_3,Y             ;Merge out movement not allowed in this phase.                             ;4    
       STA    $9B                 ;And store cooked movement.                                                ;3    
                                                                                                                   
       LDY    #$03                ;Set the delta for the ball.                                               ;2    
       LDX    #$8A                ;Point to ball's coordiates.                                               ;2    
       JSR    MoveGroundObject     ;Move the ball                                                             ;6    
       RTS                                                                                                   ;6    
                                                                                                                   
;Joystick Merge Values                                                                                             
ReadStick_3:
       .byte $00,$C0,$30            ;No change, No horizontal, No vertical.                                        
                                                                                                                   
;Deal with object pickup and putdown. 
PickupPutdown:                                                                             
       ROL    INPT4               ;Get joystick trigger.                                                     ;5    
       ROR    $D7                 ;Merget into joystick record.                                              ;5    
       LDA    $D7                 ;Get joystick record.                                                      ;3    
       AND    #$C0                ;Merget out previous presses.                                              ;2    
       CMP    #$40                ;Was it previously pressed?                                                ;2    
       BNE    PickupPutdown_2     ;If not branch.                                                            ;2    
                                                                                                                   
       LDA    #$A2                                                                                           ;2    
       CMP    $9D                 ;If nothing is being carried                                               ;3    
       BEQ    PickupPutdown_2     ;      then branch.                                                        ;2    
                                                                                                                   
       STA    $9D                 ;Drop object.                                                              ;3    
       LDA    #$04                ;Set noise type to four.                                                   ;2    
       STA    $E0                                                                                            ;3    
       LDA    #$04                ;Set noise count to four.                                                  ;2    
       STA    $DF                                                                                            ;3    

PickupPutdown_2:                                                                                                                   
       LDA    #$FF                ;????                                                                      ;2    
       STA    $98                                                                                            ;3    
                                                                                                                   
;Check for collision.                                                                                              
       LDA    CXP0FB                                                                                         ;3    
       AND    #$40                ;Get Ball-Player00 collision.                                              ;2    
       BEQ    PickupPutdown_3     ;If nothing occured then branch.                                           ;2    
                                                                                                                   
;With Player00                                                                                                     
       LDA    $95                 ;Get type of Player00                                                      ;3    
       STA    $97                 ;And Store.                                                                ;3    
       JMP    CollisionDetected   ;Deal with collision.                                                      ;3    

PickupPutdown_3:                                                                                                                   
       LDA    CXP1FB                                                                                         ;3    
       AND    #$40                ;Get Ball-Player01 collision.                                              ;2    
       BEQ    PickupPutdown_4     ;If nothing has happened, branch.                                          ;2    
                                                                                                                   
       LDA    $96                 ;Get type of Player01                                                      ;3    
       STA    $97                 ;      and store.                                                          ;3    
       JMP    CollisionDetected   ;Deal with collision.                                                      ;3    

PickupPutdown_4:                                                                                                                   
       JMP    NoObject            ;Deal with no collision (return).                                          ;3    
                                                                                                                   
;Collision occured.    
CollisionDetected:                                                                                            
       LDX    $97                 ;Get the object collided with.                                             ;3    
       JSR    GetObjectAddress    ;Get it's dynamic information.                                             ;6    
       LDA    $97                 ;Get the object collided with.                                             ;3    
       CMP    #$51                ;Is it carriable?                                                          ;2    
       BCC    NoObject            ;If not, branch.                                                           ;2    
                                                                                                                   
       LDY    #$00                                                                                           ;2    
       LDA    ($93),Y             ;Get the object's room.                                                    ;5    
       CMP    $8A                 ;Is it in the current room?                                                ;3    
       BNE    NoObject            ;If not, branch.                                                           ;2    
                                                                                                                   
       LDA    $97                 ;Get the object collided with.                                             ;3    
       CMP    $9D                 ;Is it the object being carried?                                           ;3    
       BEQ    PickupObject        ;If so, branch (and actually pick it up.)                                  ;2    
                                                                                                                   
       LDA    #$05                ;Set noise type to five.                                                   ;2    
       STA    $E0                                                                                            ;3    
       LDA    #$04                ;Set noise type to four.                                                   ;2    
       STA    $DF                                                                                            ;3    

PickupObject:                                                                                                                   
       LDA    $97                 ;Set the object as being                                                   ;3    
       STA    $9D                 ;      carried.                                                            ;3    
                                                                                                                   
       LDX    $93                 ;Get the dynamice address low byte.                                        ;3    
       LDY    #$06                                                                                           ;2    
       LDA    $99                 ;????                                                                      ;3    
       JSR    MoveObjectDelta     ;????                                                                      ;6    
                                                                                                                   
       LDY    #$01                                                                                           ;2    
       LDA    ($93),Y             ;Get the object's X coordinate.                                            ;5    
       SEC                                                                                                   ;2    
       SBC    $8B                 ;Subtract the ball's X coordinate.                                         ;3    
       STA    $9E                 ;      and store the difference.                                           ;3    
       LDY    #$02                                                                                           ;2    
       LDA    ($93),Y             ;Get the object's Y coordinate.                                            ;5    
       SEC                                                                                                   ;2    
       SBC    $8C                 ;Subtract the Ball's Y coordinate.                                         ;3    
       STA    $9F                 ;      and store the difference.                                           ;3    
                                                                                                                   
;No collision
NoObject:                                                                                                      
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Move the carried object
MoveCarriedObject:                                                                                           
       LDX    $9D                 ;Get the object being carried.                                             ;3    
       CPX    #$A2                ;If nothing then branch (return)                                           ;2    
       BEQ    MoveCarriedObject_2                                                                            ;2    
                                                                                                                   
       JSR    GetObjectAddress    ;Get it's dynamic information.                                             ;6    
       LDY    #$00                                                                                           ;2    
                                                                                                                   
       LDA    $8A                 ;Get the current room.                                                     ;3    
       STA    ($93),Y             ;      and stroe the object's current room.                                ;6    
       LDY    #$01                                                                                           ;2    
                                                                                                                   
       LDA    $8B                 ;Get the ball's X coordinate.                                              ;3    
       CLC                                                                                                   ;2    
       ADC    $9E                 ;Add the X difference.                                                     ;3    
       STA    ($93),Y             ;      and store as the object's X coordinate.                             ;6    
       LDY    #$02                                                                                           ;2    
       LDA    $8C                 ;Get the ball's Y coordinate.                                              ;3    
       CLC                                                                                                   ;2    
       ADC    $9F                 ;Add the Y difference.                                                     ;3    
       STA    ($93),Y             ;      and store as the object's Y coordinate.                             ;6    
                                                                                                                   
       LDY    #$00                ;Set no delta.                                                             ;2    
       LDA    #$FF                ;Set no movement.                                                          ;2    
       LDX    $93                 ;Get the object's dynamic address.                                         ;3    
       JSR    MoveGroundObject     ;Move the object.                                                          ;6    
MoveCarriedObject_2:
       RTS                                                                                                   ;6    
                                                                                                                   
;Move the object. 
MoveGroundObject:                                                                         
       JSR    MoveObjectDelta     ;Move the object by delta.                                                 ;6    
       LDY    #$02                ;Set to do the three                                                       ;2    
MoveGroundObject_2:
       STY    $9A                 ;      portcullises.                                                       ;3    
       LDA.wy $00C8,Y             ;Get the portal state.                                                     ;4    
       CMP    #$1C                ;Is it in a closed state?                                                  ;2    
       BEQ    GetPortal           ;If not, next portal.                                                      ;2    
                                                                                                                   
;Deal with object moving out of a castle.                                                                          
       LDY    $9A                 ;Get port number.                                                          ;3    
       LDA    VSYNC,X             ;Get object's room number.                                                 ;4    
       CMP    EntryRoomOffsets,Y  ;Is it in a castle entry room.                                             ;4    
       BNE    GetPortal           ;If not, next portal.                                                      ;2    
                                                                                                                   
       LDA    WSYNC,X             ;Get the object's Y coordinate.                                            ;4    
       CMP    #$0D                ;Is it above &OD i.e at the bottom.                                        ;2    
       BPL    GetPortal           ;If so then branch.                                                        ;2    
                                                                                                                   
       LDA    CastleRoomOffsets,Y ;Get the castle room.                                                      ;4    
       STA    VSYNC,X             ;And put the object in the castle room.                                    ;4    
       LDA    #$50                                                                                           ;2    
       STA    VBLANK,X            ;Set the object's new X coordinate.                                        ;4    
       LDA    #$2C                                                                                           ;2    
       STA    WSYNC,X             ;Set the new object's Y coordinate.                                        ;4    
       LDA    #$01                                                                                           ;2    
       STA.wy $00C8,Y             ;Set the portcullis state to 01.                                           ;5    
       RTS                                                                                                   ;6    

GetPortal:                                                                                                                   
       LDY    $9A                 ;Get the portcullis number.                                                ;3    
       DEY                        ;      goto next,                                                          ;2    
       BPL    MoveGroundObject_2   ;      and continue.                                                       ;2    
                                                                                                                   
;Check and Deal with Up.                                                                                           
       LDA    WSYNC,X             ;Get the object's Y coordinate.                                            ;4    
       CMP    #$6A                ;Has it reched above the top.                                              ;2    
       BMI    DealWithLeft        ;If not, branch.                                                           ;2    
                                                                                                                   
       LDA    #$0D                ;Set new Y coordinate to bottom.                                           ;2    
       STA    WSYNC,X                                                                                        ;4    
       LDY    #$05                ;Get the direction wanted.                                                 ;2    
       JMP    GetNewRoom          ;Go and get new room.                                                      ;3    
                                                                                                                   
;Check and Deal with Left.
DealWithLeft:                                                                                         
       LDA    VBLANK,X            ;Get the object's X coordinate.                                            ;4    
       CMP    #$03                ;Is it Three or less?                                                      ;2    
       BCC    DealWithLeft_2      ;IF so, branch.  (off to left)                                             ;2    
                                                                                                                   
       CMP    #$F0                ;Is it's &F0 or more.                                                      ;2    
       BCS    DealWithLeft_2      ;If so, branch.  (off to right)                                            ;2    
                                                                                                                   
       JMP    DealWithDown                                                                                          ;3    

DealWithLeft_2:                                                                                                                   
       CPX    #$8A                ;Are we dealling with the ball?                                            ;2    
       BEQ    DealWithLeft_3      ;If so Branch.                                                             ;2    
                                                                                                                   
       LDA    #$9A                ;Set new X coordinate for the others.                                      ;2    
       JMP    DealWithLeft_4                                                                                          ;3    

DealWithLeft_3:                                                                                                                   
       LDA    #$9E                ;Set new X coordinate for the ball.                                        ;2    

DealWithLeft_4:                                                                                                                   
       STA    VBLANK,X            ;Store the next X coordinate.                                              ;4    
       LDY    #$08                ;And get the direction wanted.                                             ;2    
       JMP    GetNewRoom          ;Go and get new room.                                                      ;3    
                                                                                                                   
;Check and Deal with Down.                                                                                         
DealWithDown:
       LDA    WSYNC,X             ;Get object's Y coordinate.                                                ;4    
       CMP    #$0D                ;If it's greater than &0D then                                             ;2    
       BCS    DealWithRight       ;Branch.                                                                   ;2    
                                                                                                                   
       LDA    #$69                ;Set new Y coordinate.                                                     ;2    
       STA    WSYNC,X                                                                                        ;4    
       LDY    #$07                ;Get the direction wanted.                                                 ;2    
       JMP    GetNewRoom          ;Go and get new room.                                                      ;3    
                                                                                                                   
;Check and Deal with Right.  
DealWithRight:
       LDA    VBLANK,X            ;Get the object's X coordinate.                                            ;4    
       CPX    #$8A                ;Are we dealing with the ball.                                             ;2    
       BNE    DealWithRight_2     ;Branch if not.                                                            ;2    
                                                                                                                   
       CMP    #$9F                ;Has the object reached the right?                                         ;2    
       BCC    MovementReturn      ;Branch if not.                                                            ;2    
                                                                                                                   
       LDA    VSYNC,X             ;Get the Ball's Room.                                                      ;4    
       CMP    #$03                ;Is it room #3 (Right to secret room)                                      ;2    
       BNE    DealWithRight_3     ;Branch if not.                                                            ;2    
                                                                                                                   
       LDA    $A1                 ;Check the room of the black dot.                                          ;3    
       CMP    #$15                ;Is it in the hidden room area?                                            ;2    
       BEQ    DealWithRight_3     ;If so, Branch.                                                            ;2    
                                                                                                                   
;Manually change to secret room.                                                                                   
       LDA    #$1E                ;Set room to secret room.                                                  ;2    
       STA    VSYNC,X             ;And make it current.                                                      ;4    
       LDA    #$03                ;Set the X coordinate.                                                     ;2    
       STA    VBLANK,X                                                                                       ;4    
       JMP    MovementReturn      ;And Exit.                                                                 ;3    

DealWithRight_2:                                                                                                                   
       CMP    #$9B                ;Has the object reached the right of the screen?                           ;2    
       BCC    MovementReturn      ;Branch if not (no room change)                                            ;2    

DealWithRight_3:                                                                                                                   
       LDA    #$03                ;Set the next X coordinate.                                                ;2    
       STA    VBLANK,X                                                                                       ;4    
       LDY    #$06                ;And get the direction wanted.                                             ;2    
       JMP    GetNewRoom          ;Get the new room.                                                         ;3    
                                                                                                                   
;Get new room 
GetNewRoom:                                                                                                     
       LDA    VSYNC,X             ;Get the object's room.                                                    ;4    
       JSR    RoomNumToAddress    ;Convert it to an address.                                                 ;6    
       LDA    ($93),Y             ;Get the adjacent room.                                                    ;5    
       JSR    AdjustRoomLevel     ;Deal with the level differences.                                          ;6    
       STA    VSYNC,X             ;      and store as new object's room.                                     ;4    

MovementReturn:                                                                                                                   
       RTS                                                                                                   ;6    
                                                                                                                   
;Move the object in direction by delta.                                                                            
MoveObjectDelta:
       STA    $9B                 ;Stored direction wanted.                                                  ;3    
MoveObjectDelta_2:
       DEY                        ;Count down the delta.                                                     ;2    
       BMI    MoveObjectDelta_7                                                                              ;2    
                                                                                                                   
       LDA    $9B                 ;Get direction wanted.                                                     ;3    
       AND    #$80                ;Check for right move.                                                     ;2    
       BNE    MoveObjectDelta_3   ;If no move right then branch.                                             ;2    
                                                                                                                   
       INC    VBLANK,X            ;Increment the X coordinate.                                               ;6    

MoveObjectDelta_3:                                                                                                                   
       LDA    $9B                 ;Get the direction wanted.                                                 ;3    
       AND    #$40                ;Check for left move.                                                      ;2    
       BNE    MoveObjectDelta_4   ;If no move left then branch.                                              ;2    
                                                                                                                   
       DEC    VBLANK,X            ;Decrement the X coordinate.                                               ;6    

MoveObjectDelta_4:                                                                                                                   
       LDA    $9B                 ;Get the direction wanted.                                                 ;3    
       AND    #$10                ;Check for move up.                                                        ;2    
       BNE    MoveObjectDelta_5   ;If no move up then branch.                                                ;2    
       INC    WSYNC,X                                                                                        ;6    

MoveObjectDelta_5:                                                                                                                   
       LDA    $9B                 ;Get direction wanted.                                                     ;3    
       AND    #$20                ;Check for move down.                                                      ;2    
       BNE    MoveObjectDelta_6   ;If no move down the branch.                                               ;2    
                                                                                                                   
       DEC    WSYNC,X             ;Decrement the Y coordinate.                                               ;6    
     
MoveObjectDelta_6:                                                                                                              
       JMP    MoveObjectDelta_2   ;Keep going until delta finished.                                          ;3    
MoveObjectDelta_7:
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Adjust room for different levels.                                                                                 
AdjustRoomLevel:
       CMP    #$80                ;Is the room number                                                        ;2    
       BCC    AdjustRoomLevel_2   ;      above &80?                                                          ;2    
                                                                                                                   
       SEC                                                                                                   ;2    
       SBC    #$80                ;Remove the &80 flag and                                                   ;2    
       STA    $D8                 ;      store the room number.                                              ;3    
       LDA    $DD                 ;Get the level number.                                                     ;3    
       LSR                        ;Devide it by two.                                                         ;2    
       CLC                                                                                                   ;2    
       ADC    $D8                 ;Add to the original room.                                                 ;3    
       TAY                                                                                                   ;2    
       LDA    RoomDiffs,Y             ;Use as an offset to get the next room.                                    ;4    
AdjustRoomLevel_2:                                                                                                                   
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Get player-ball collision.                                                                                        
PBCollision:
       CMP    $95                 ;Is it the rist object?                                                    ;3    
       BEQ    PBCollision_2       ;YES - Then Branch.                                                        ;2    
                                                                                                                   
       CMP    $96                 ;Is it the second object?                                                  ;3    
       BEQ    PBCollision_3       ;YES - Then Branch.                                                        ;2    
                                                                                                                   
       LDA    #$00                ;Otherewise nothing.                                                       ;2    
       RTS                                                                                                   ;6    

PBCollision_2:                                                                                                                   
       LDA    CXP0FB              ;Get player00-ball collision.                                              ;3    
       AND    #$40                                                                                           ;2    
       RTS                                                                                                   ;6    
PBCollision_3:                                                                                                                   
       LDA    CXP1FB              ;Get player01-ball collision.                                              ;3    
       AND    #$40                                                                                           ;2    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Find which object has hit object wanted. 
FindObjHit:                                                                         
       LDA    CXPPMM              ;Get Player00-Player01                                                     ;3    
       AND    #$80                ;      collision.                                                          ;2    
       BEQ    FindObjHit_2        ;If nothing, Branch.                                                       ;2    
                                                                                                                   
       CPX    $95                 ;Is object 1 the one being hit?                                            ;3    
       BEQ    FindObjHit_3        ;If so, Branch.                                                            ;2    
                                                                                                                   
       CPX    $96                 ;Is object 2 the one being hit?                                            ;3    
       BEQ    FindObjHit_4        ;If so, Branch.                                                            ;2    
FindObjHit_2:                                                                                                                   
       LDA    #$A2                ;Therefore select the other.                                               ;2    
       RTS                                                                                                   ;6    
FindObjHit_3:                                                                                                                   
       LDA    $96                 ;Therefore select the other.                                               ;3    
       RTS                                                                                                   ;6    
FindObjHit_4:                                                                                                                   
       LDA    $95                 ;Therefore select the other.                                               ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Move object.
MoveGameObject:                                                                                                      
       JSR    GetLinkedObject     ;Get liked object and movement.                                            ;6    
       LDX    $D5                 ;Get dynamic data address.                                                 ;3    
       LDA    $9B                 ;Get Movement.                                                             ;3    
       BNE    MoveGameObject_2    ;If movement then branch.                                                  ;2    
                                                                                                                   
       LDA    RSYNC,X             ;Use old movement.                                                         ;4    

MoveGameObject_2:                                                                                                                   
       STA    RSYNC,X             ;Stoe the new movement.                                                    ;4    
       LDY    $D4                 ;Get the object's Delta.                                                   ;3    
       JSR    MoveGroundObject     ;Move the object.                                                          ;6    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Find liked object and get movement.
GetLinkedObject:                                                                               
       LDA    #$00                ;Set index to zero.                                                        ;2    
       STA    $E1                                                                                            ;3    
GetLinkedObject_2:                                                                                                                   
       LDY    $E1                 ;Get index.                                                                ;3    
       LDA    ($D2),Y             ;Get first object.                                                         ;5    
       TAX                                                                                                   ;2    
       INY                                                                                                   ;2    
       LDA    ($D2),Y             ;Get second object.                                                        ;5    
       TAY                                                                                                   ;2    
       LDA    VSYNC,X             ;Get object1;s room.                                                       ;4    
       CMP.wy $0000,Y             ;Combare with object2's room.                                              ;4    
       BNE    GetLinkedObject_3   ;If not the same room then branch.                                         ;2    
                                                                                                                   
       CPY    $D6                 ;Have we matched the second object                                         ;3    
       BEQ    GetLinkedObject_3   ;      for difficulty (if so, carry on).                                   ;2    
                                                                                                                   
       CPX    $D6                 ;Have we matched the first object                                          ;3    
       BEQ    GetLinkedObject_3   ;      for difficulty (if so, carry on).                                   ;2    
                                                                                                                   
       JSR    GetLinkedObject_4   ;Get object's movement.                                                    ;6    
       RTS                                                                                                   ;6    
GetLinkedObject_3:                                                                                                                   
       INC    $E1                 ;Increment the index.                                                      ;5    
       INC    $E1                                                                                            ;5    
       LDY    $E1                 ;Get the index number.                                                     ;3    
       LDA    ($D2),Y             ;Check for end of sequence.                                                ;5    
       BNE    GetLinkedObject_2   ;If not branch.                                                            ;2    
                                                                                                                   
       LDA    #$00                ;Set no move if no                                                         ;2    
       STA    $9B                 ;      liked object is found.                                              ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Work out object's movement.                                                                                       
GetLinkedObject_4:
       LDA    #$FF                ;Set object movement to none.                                              ;2    
       STA    $9B                                                                                            ;3    
                                                                                                                   
       LDA.wy $0000,Y             ;Get oject2's room.                                                        ;4    
       CMP    VSYNC,X             ;Compare it with object's room.                                            ;4    
       BNE    GetLinkedObject_8   ;If not the same, forget it.                                               ;2    
                                                                                                                   
       LDA.wy $0001,Y             ;Get Object2's X coordinate.                                               ;4    
       CMP    VBLANK,X            ;Get Object1;s X coordinate.                                               ;4    
       BCC    GetLinkedObject_5   ;If Object2 to left of Object1 then Branch.                                ;2    
       BEQ    GetLinkedObject_6   ;If Object2 on Object1 then Branch.                                        ;2    
                                                                                                                   
       LDA    $9B                 ;Get Object Movement.                                                      ;3    
       AND    #$7F                ;Signal a move right.                                                      ;2    
       STA    $9B                                                                                            ;3    
       JMP    GetLinkedObject_6   ;Now try Vertical.                                                         ;3    
GetLinkedObject_5:                                                                                                                   
       LDA    $9B                 ;Get object movent.                                                        ;3    
       AND    #$BF                ;Signal a move left.                                                       ;2    
       STA    $9B                                                                                            ;3    

GetLinkedObject_6:                                                                                                                   
       LDA.wy $0002,Y             ;Get Object2's Y Coordinate.                                               ;4    
       CMP    WSYNC,X             ;Get Object1's X Coordinate.                                               ;4    
       BCC    GetLinkedObject_7   ;If Object2 Below Object1 Then Branch.                                     ;2    
       BEQ    GetLinkedObject_8   ;If Object2 on Object1 Then Branch.                                        ;2    
                                                                                                                   
       LDA    $9B                 ;Get Object Movement.                                                      ;3    
       AND    #$EF                ;Signal a move up.                                                         ;2    
       STA    $9B                                                                                            ;3    
       JMP    GetLinkedObject_8   ;Jump to Finish.                                                           ;3    

GetLinkedObject_7:                                                                                                                   
       LDA    $9B                 ;Get Object Movement.                                                      ;3    
       AND    #$DF                ;Signal a move down.                                                       ;2    
       STA    $9B                                                                                            ;3    

GetLinkedObject_8:                                                                                                                   
       LDA    $9B                 ;Get the Move.                                                             ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Move the Red Dragon                                                                                               
MoveRedDragon:
       LDA    #<RedDragMatrix                                                                                           ;2    
       STA    $D2                 ;Set the Low address of Object Store.                                      ;3    
       LDA    #>RedDragMatrix                                                                                           ;2    
       STA    $D3                 ;Set the High address of Object Store.                                     ;3    
       LDA    #$03                                                                                           ;2    
       STA    $D4                 ;Set the Dragon's Delta                                                    ;3    
       LDX    #$36                ;Select Dragon #1 : Red                                                    ;2    
       JSR    MoveDragon                                                                                     ;6    
       RTS                                                                                                   ;6    
                                                                                                                   
;Red Dragon Object Matrix                                                                                          
RedDragMatrix:
       .byte $B6,$A4                  ;Sword, Red Dragon                                                           
       .byte $A4,$8A                  ;Red Dragon, Ball                                                            
       .byte $A4,$B9                  ;Red Dragon, Chalise                                                         
       .byte $A4,$C2                  ;Red Dragon, White Key                                                       
       .byte $00                                                                                                   
                                                                                                                   
;Move the Yellow Dragon.                                                                                           
MoveYellowDragon:
       LDA    #<YelDragMatrix                                                                                           ;2    
       STA    $D2                 ;Set the Low Address of Object Store.                                      ;3    
       LDA    #>YelDragMatrix                                                                                           ;2    
       STA    $D3                 ;Set the High Address of Object Store.                                     ;3    
       LDA    #$02                                                                                           ;2    
       STA    $D4                 ;Set the Yellow Dragon's Delta.                                            ;3    
       LDX    #$3F                                                                                           ;2    
       JSR    MoveDragon          ;Select Dragon #2 : Yellow.                                                ;6    
       RTS                                                                                                   ;6    
                                                                                                                   
;Yellow Dragon's Object Matrix                                                                                     
YelDragMatrix:
	 .byte $B6,$A9                  ;Sword, Yellow Dragon                                                        
       .byte $BF,$A9                  ;Yellow Key, Yellow Dragon                                                   
       .byte $A9,$8A                  ;Yellow Dragon, Ball                                                         
       .byte $A9,$B9                  ;Yellow Dragon, Chalise                                                      
       .byte $00                                                                                                   
                                                                                                                   
                                                                                                                   
;Move the Green Dragon                                                                                             
MoveGreenDragon:
       LDA    #<GreenDragonMatrix                                                                                           ;2    
       STA    $D2                 ;Set Low Address of Object Store.                                          ;3    
       LDA    #>GreenDragonMatrix                                                                                           ;2    
       STA    $D3                 ;Set High Address of Object Store.                                         ;3    
       LDA    #$02                                                                                           ;2    
       STA    $D4                 ;Set the Green Dragon's Delta.                                             ;3    
       LDX    #$48                ;Select Dragon #3 : Green                                                  ;2    
       JSR    MoveDragon                                                                                     ;6    
       RTS                                                                                                   ;6    
                                                                                                                   
;Green Dragon's Object Matrix                                                                                      
GreenDragonMatrix:
       .byte $B6,$AE                  ;Sword, Green Dragon                                                         
       .byte $AE,$8A                  ;Green Dragon, Ball                                                          
       .byte $AE,$B9                  ;Green Dragon Chalise                                                        
       .byte $AE,$BC                  ;Green Dragon, Bridge                                                        
       .byte $AE,$B3                  ;Green Dragon, Magnet                                                        
       .byte $AE,$C5                  ;Green Dragon, Black Key                                                     
       .byte $00                                                                                                   
                                                                                                                   
                                                                                                                   
;Move A Dragon
MoveDragon:                                                                                                       
       STX    $A0                 ;Save Object were dealing with.                                            ;3    
       LDA    Store1,X             ;Get the Object's Dynamic Data.                                            ;4    
       TAX                                                                                                   ;2    
       LDA    NUSIZ0,X            ;Get the Object's State.                                                   ;4    
       CMP    #$00                ;Is it in State 00 (Normal #1)                                             ;2    
       BNE    MoveDragon_6        ;Branch if not.                                                            ;2    
                                                                                                                   
;Dragon Normal (State 1)                                                                                           
       LDA    SWCHB               ;Read console switches.                                                    ;4    
       AND    #$80                ;Check for P1 difficulty.                                                  ;2    
       BEQ    MoveDragon_2        ;If Amateur Branch.                                                        ;2    
                                                                                                                   
       LDA    #$00                ;Set Hard - Ignore Nothing                                                 ;2    
       JMP    MoveDragon_3                                                                                   ;3    

MoveDragon_2:                                                                                                                   
       LDA    #$B6                ;Set Easy - Ignore Sword.                                                  ;2    

MoveDragon_3:                                                                                                                   
       STA    $D6                 ;Store Difficulty                                                          ;3    
       STX    $D5                 ;Store Dynamic Data Address.                                               ;3    
       JSR    MoveGameObject                                                                                 ;6    
                                                                                                                   
       LDA    $A0                 ;Get Object                                                                ;3    
       JSR    PBCollision         ;      And get the Player-Ball collision.                                  ;6    
       BEQ    MoveDragon_4        ;If None Then Branch.                                                      ;2    
                                                                                                                   
       LDA    SWCHB               ;Get Console Switched.                                                     ;4    
       ROL                        ;Move P0 difficulty to                                                     ;2    
       ROL                        ;      bit 01 position.                                                    ;2    
       ROL                                                                                                   ;2    
       AND    #$01                ;Mask it out.                                                              ;2    
       ORA    $DD                 ;Merget in the Level Number.                                               ;3    
       TAY                        ;Create Lookup.                                                            ;2    
       LDA    DragonDiff,Y             ;Get New State.                                                            ;4    
       STA    NUSIZ0,X            ;Store as Dragon's State (Open Mouthed).                                   ;4    
       LDA    $E3                                                                                            ;3    
       STA    VBLANK,X            ;Get Temp Ball X Coord and Store as Dragon's.                              ;4    
       LDA    $E4                                                                                            ;3    
       STA    WSYNC,X             ;Get Temp Ball Y Coord and Store as Dragon's                               ;4    
       LDA    #$01                                                                                           ;2    
       STA    $E0                 ;Set Noise Type to 01                                                      ;3    
       LDA    #$10                                                                                           ;2    
       STA    $DF                 ;Set Noise Count to 10 i.e. make roar noise.                               ;3    

MoveDragon_4:                                                                                                                  
       STX    $9A                 ;Store Object's Dynamic Data Address.                                      ;3    
       LDX    $A0                 ;Get the Object Number.                                                    ;3    
       JSR    FindObjHit          ;See if anoher object has hit the dragon.                                  ;6    
       LDX    $9A                 ;Get the Object Address.                                                   ;3    
       CMP    #$51                ;Has the Sword hit the Dragon?                                             ;2    
       BNE    MoveDragon_5        ;If Not, Branch.                                                           ;2    
                                                                                                                   
       LDA    #$01                ;Set the State to 01 (Dead)                                                ;2    
       STA    NUSIZ0,X                                                                                       ;4    
       LDA    #$03                ;Set Sound Three.                                                          ;2    
       STA    $E0                                                                                            ;3    
       LDA    #$10                ;Set a Noise count of &10.                                                 ;2    
       STA    $DF                                                                                            ;3    

MoveDragon_5:                                                                                                                   
       JMP    MoveDragon_9        ;Jump to Finish.                                                           ;3    

MoveDragon_6:                                                                                                                   
       CMP    #$01                ;Is it in State 01 (Dead)                                                  ;2    
       BEQ    MoveDragon_9        ;Branch if So (Return)                                                     ;2    
                                                                                                                   
       CMP    #$02                ;Is it int State 02 (Normal #2)                                            ;2    
       BNE    MoveDragon_7        ;Branch if Not.                                                            ;2    
                                                                                                                   
;Normal Dragon State 2 (Eaten Ball)                                                                                
       LDA    VSYNC,X             ;Get the Dragon's Current Room.                                            ;4    
       STA    $8A                 ;Store as the Ball's Current Room                                          ;3    
       STA    $E2                 ;      and Previous Room.                                                  ;3    
       LDA    VBLANK,X            ;Get the Dragon's X Coordinate.                                            ;4    
       CLC                                                                                                   ;2    
       ADC    #$03                ;Adjust                                                                    ;2    
       STA    $8B                 ;      and store as the ball's X coordinate.                               ;3    
       STA    $E3                 ;      and previous X coordinate.                                          ;3    
       LDA    WSYNC,X             ;Get the Dragon's Y coordinate.                                            ;4    
       SEC                                                                                                   ;2    
       SBC    #$0A                ;Adjust                                                                    ;2    
       STA    $8C                 ;      and store as the ball's Y coordinate.                               ;3    
       STA    $E4                 ;      and the previous Y coordinate.                                      ;3    
       JMP    MoveDragon_9                                                                                   ;3    
                                                                                                                   
                                                                                                                   
;Dragon Roaring.                                                                                                   
MoveDragon_7:
       INC    NUSIZ0,X            ;Increment the Dragon's State.                                             ;6    
       LDA    NUSIZ0,X            ;Get it's State.                                                           ;4    
       CMP    #$FC                ;Is it near the end?                                                       ;2    
       BCC    MoveDragon_9        ;If Not, Branch.                                                           ;2    
                                                                                                                   
       LDA    $A0                 ;Get the Dragon's Number.                                                  ;3    
       JSR    PBCollision         ;Check if the Ball is colliding.                                           ;6    
       BEQ    MoveDragon_9        ;If not, Branch.                                                           ;2    
                                                                                                                   
       LDA    #$02                ;Set the State to State 02 : Eaten                                         ;2    
       STA    NUSIZ0,X                                                                                       ;4    
       LDA    #$02                ;Set noise two.                                                            ;2    
       STA    $E0                                                                                            ;3    
       LDA    #$10                ;Set the Count of Noise to &10.                                            ;2    
       STA    $DF                                                                                            ;3    
       LDA    #$9B                ;Get the Maximum X Coordinate.                                             ;2    
       CMP    VBLANK,X            ;Compare with the Dragon's X Coordinate.                                   ;4    
       BEQ    MoveDragon_8                                                                                          ;2    
       BCS    MoveDragon_8                                                                                          ;2    
                                                                                                                   
       STA    VBLANK,X            ;If too large then Use It.                                                 ;4    

MoveDragon_8:                                                                                                                   
       LDA    #$17                ;Set Minimum Y Coordinate.                                                 ;2    
       CMP    WSYNC,X             ;Compare with the Dragon's Y Coordinate.                                   ;4    
       BCC    MoveDragon_9                                                                                   ;2    
                                                                                                                   
       STA    WSYNC,X             ;If Too Small, set as Dragon's Y coordinate.                               ;4    
MoveDragon_9:                                                                                                                   
       RTS                                                                                                   ;6    
                                                                                                                   
;Dragon Difficulty                                                                                                 
DragonDiff:
       .byte $D0,$E8                  ;Level 1 : Am, Pro                                                           
       .byte $F0,$F6                  ;Level 2 : Am, Pro                                                           
       .byte $F0,$F6                  ;Level 3 : Am, Pro                                                           
                                                                                                                   
;Move Bat                                                                                                          
MoveBat:
       INC    $CF                 ;Put Bat in the Next State.                                                ;5    
       LDA    $CF                 ;Get the Bat State.                                                        ;3    
       CMP    #$08                ;Has it Reached the Maximum?                                               ;2    
       BNE    MoveBat_2                                                                                      ;2    
                                                                                                                   
       LDA    #$00                ;If So, Reset the Bat State.                                               ;2    
       STA    $CF                                                                                            ;3    
MoveBat_2:                                                                                                                  
       LDA    $D1                 ;Get the Bat Fed-Up Value.                                                 ;3    
       BEQ    MoveBat_3           ;If Bat Fed-Up then Branch.                                                ;2    
                                                                                                                   
       INC    $D1                 ;Increment its value for next time.                                        ;5    
       LDA    $CE                 ;Get the Bat's Movement.                                                   ;3    
       LDX    #$CB                ;Position to Bat.                                                          ;2    
       LDY    #$03                ;Get the Bat's Deltas.                                                     ;2    
       JSR    MoveGroundObject    ;Move the Bat.                                                             ;6    
       JMP    MoveBat_4           ;Update the Bat's Object.                                                  ;3    
                                                                                                                   
;Bat Fed-Up 
MoveBat_3:                                                                                                       
       LDA    #$CB                ;Store the Bat's Dynamic Data Address                                      ;2    
       STA    $D5                                                                                            ;3    
       LDA    #$03                ;Set the Bat's Delta.                                                      ;2    
       STA    $D4                                                                                            ;3    
       LDA    #<BatMatrix         ;Set the Low Address of Object Store.                                      ;2    
       STA    $D2                                                                                            ;3    
       LDA    #>BatMatrix         ;Set the High Address of Object Store.                                     ;2    
       STA    $D3                                                                                            ;3    
       LDA    $D0                 ;Get Object being Carried by Bat,                                          ;3    
       STA    $D6                 ;      And Copy.                                                           ;3    
       JSR    MoveGameObject      ;Move the Bat.                                                             ;6    
                                                                                                                   
       LDY    $E1                 ;Get Object Liked Index.                                                   ;3    
       LDA    ($D2),Y             ;Look up the Object Found in the Table.                                    ;5    
       BEQ    MoveBat_4           ;If nothing found then Forget it.                                          ;2    
                                                                                                                   
       INY                                                                                                   ;2    
       LDA    ($D2),Y             ;Get the Object Wanted.                                                    ;5    
       TAX                                                                                                   ;2    
       LDA    VSYNC,X             ;Get the Object's Room.                                                    ;4    
       CMP    $CB                 ;Is it the Same as the Bats?                                               ;3    
       BNE    MoveBat_4           ;If not Forget it.                                                         ;2    
                                                                                                                   
;See if Bat Can pick up Object.                                                                                    
       LDA    VBLANK,X            ;Get the Object's X Coordinate.                                            ;4    
       SEC                                                                                                   ;2    
       SBC    $CC                 ;Find the differenct with the Bat's                                        ;3    
       CLC                        ;X coordinate.                                                             ;2    
       ADC    #$04                ;Adjust so Bat in middle of object.                                        ;2    
       AND    #$F8                ;Is Bat within Seven Pixels?                                               ;2    
       BNE    MoveBat_4           ;If not, no pickup possible.                                               ;2    
                                                                                                                   
       LDA    WSYNC,X             ;Get the Object's Y Coordinate.                                            ;4    
       SEC                                                                                                   ;2    
       SBC    $CD                 ;Find the Difference with the Bat's                                        ;3    
       CLC                        ;      Y Coordinate.                                                       ;2    
       ADC    #$04                ;Adjust.                                                                   ;2    
       AND    #$F8                ;Is the Bat within Seven Pixels?                                           ;2    
       BNE    MoveBat_4           ;If not, No Pickup Possible.                                               ;2    
                                                                                                                   
;Get Object                                                                                                        
       STX    $D0                 ;Store Object as Being Carried.                                            ;3    
       LDA    #$10                ;Reset the Bat Fed Up Time.                                                ;2    
       STA    $D1                                                                                            ;3    
                                                                                                                   
;Move Object Being Carried by Bat.                                                                                 
MoveBat_4:
       LDX    $D0                 ;Get Object Being Carried by Bat.                                          ;3    
       LDA    $CB                 ;Get the Bat's Room.                                                       ;3    
       STA    VSYNC,X             ;Store this as the Object's Room.                                          ;4    
       LDA    $CC                 ;Get the Bat's X coordinate.                                               ;3    
       CLC                                                                                                   ;2    
       ADC    #$08                ;Adjust to the Right.                                                      ;2    
       STA    VBLANK,X            ;Make it the Object's X coordinate.                                        ;4    
       LDA    $CD                 ;Get the Bat's Y Coordinate.                                               ;3    
       STA    WSYNC,X             ;Store it as the Object's Y Coordinate.                                    ;4    
       LDA    $D0                 ;Get the Object Being Carried by the Bat.                                  ;3    
       LDY    $9D                 ;Get the Object Being Carried by the Ball.                                 ;3    
       CMP    Store1,Y             ;Are the the Same?                                                         ;4    
       BNE    MoveBat_5           ;If not Branch.                                                            ;2    
                                                                                                                   
       LDA    #$A2                ;Set Nothing Being                                                         ;2    
       STA    $9D                 ;      Carried.                                                            ;3    
MoveBat_5:                                                                                                                   
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Bat Object Matrix.
BatMatrix:                                                                                                
       .byte $CB,$B9                  ;Bat,Chalise                                                                 
       .byte $CB,$B6                  ;Bat,Sword                                                                   
       .byte $CB,$BC                  ;Bat,Bridge                                                                  
       .byte $CB,$BF                  ;Bat,Yellow Key                                                              
       .byte $CB,$C2                  ;Bat,White Key                                                               
       .byte $CB,$C5                  ;Bat,Black Key                                                               
       .byte $CB,$A4                  ;Bat,Red Dragon                                                              
       .byte $CB,$A9                  ;Bat,Yellow Dragon                                                           
       .byte $CB,$AE                  ;Bat,Green Dragon                                                            
       .byte $CB,$B3                  ;Bat,Magnet                                                                  
       .byte $00                                                                                                   
                                                                                                                   
                                                                                                                   
;Deal with Portcullis and Collisions.    
Portals:                                                                          
       LDY    #$02                ;For Each Portcullis.                                                      ;2    
Portals_2:
       LDX    PortOffsets,Y             ;Get the Portcullises offset number.                                       ;4    
       JSR    FindObjHit          ;See if an Object Collided with it.                                        ;6    
       STA    $97                 ;      Store that Object.                                                  ;3    
       CMP    KeyOffsets,Y             ;Is it the Associated Key?                                                 ;4    
       BNE    Portals_3           ;If not then Branch.                                                       ;2    
                                                                                                                   
       TYA                        ;Get the Portcullis Number                                                 ;2    
       TAX                                                                                                   ;2    
       INC    $C8,X               ;Change it's state to open it.                                             ;6    
Portals_3:                                                                                                                   
       TYA                        ;Get the Porcullis number.                                                 ;2    
       TAX                                                                                                   ;2    
       LDA    $C8,X               ;Get the State.                                                            ;4    
       CMP    #$1C                ;Is it Closed?                                                             ;2    
       BEQ    Portals_7           ;Yes - then Branch.                                                        ;2    
                                                                                                                   
       LDA    PortOffsets,Y             ;Get Portcullis number.                                                    ;4    
       JSR    PBCollision         ;Get the Player-Ball Collision.                                            ;6    
       BEQ    Portals_4           ;If Not Then Branch.                                                      ;2    
                                                                                                                   
       LDA    #$01                ;Set the Portcullis to Closed.                                             ;2    
       STA    $C8,X                                                                                          ;4    
       LDX    #$8A                ;Set to the Castle.                                                        ;2    
       JMP    Portals_6           ;Put the Ball in the Castle.                                               ;3    

Portals_4:                                                                                                                   
       LDA    $97                 ;Get the Object that hit the Portcullis.                                   ;3    
       CMP    #$A2                ;Is it nothing?                                                            ;2    
       BEQ    Portals_5           ;If so, Branch.                                                            ;2    
                                                                                                                   
       LDX    $97                 ;Get Object.                                                               ;3    
       STY    $9A                 ;Save Y                                                                    ;3    
       JSR    GetObjectAddress    ;And find it's Dynamic Address.                                            ;6    
       LDY    $9A                 ;Retrieve Y                                                                ;3    
       LDX    $93                 ;Get Object's Address.                                                     ;3    
       JMP    Portals_6           ;Put Object In the Castle.                                                 ;3    
     
Portals_5:                                                                                                              
       JMP    Portals_7                                                                                      ;3    

Portals_6:                                                                                                                   
       LDA    EntryRoomOffsets,Y ;Look up Castle endry room for this port.                                        ;4    
       STA    VSYNC,X       ;Make it the object's Room.                                                      ;4    
       LDA    #$10          ;Give the Object a new Y coordinate.                                             ;2    
       STA    WSYNC,X                                                                                        ;4    
Portals_7:                                                                                                                   
       TYA                  ;Get the Portcullis number.                                                      ;2    
       TAX                                                                                                   ;2    
       LDA    $C8,X         ;Get its State.                                                                  ;4    
       CMP    #$01          ;Is it Open?                                                                     ;2    
       BEQ    Portals_8     ;Yes - Then Branch.                                                              ;2    
                                                                                                                   
       CMP    #$1C          ;Is it Closed?                                                                   ;2    
       BEQ    Portals_8     ;Yes - Then Branch.                                                              ;2    
                                                                                                                   
       INC    $C8,X         ;Increment it's State.                                                           ;6    
       LDA    $C8,X         ;Get the State.                                                                  ;4    
       CMP    #$38          ;Has it reached the maximum state.                                               ;2    
       BNE    Portals_8     ;If not, Branch.                                                                 ;2    
                                                                                                                   
       LDA    #$01          ;Set to Closed                                                                   ;2    
       STA    $C8,X         ;      State.                                                                    ;4    

Portals_8:                                                                                                                   
       DEY                  ;Goto the next portcullis.                                                       ;2    
       BMI    Portals_9     ;Branch if Finished.                                                             ;2    
       JMP    Portals_2     ;Do next Protcullis.                                                             ;3    
Portals_9:
       RTS                                                                                                   ;6    



                                                                                                                   
;Portcullis #1, #2, #3
PortOffsets:                                                                                             
	 .byte $09,$12,$1B                                                                                           
                                                                                                                   
;Keys #1, #2, #3  (Yellow, White, Black)                                                                           
KeyOffsets:
	 .byte $63,$6C,$75                                                                                           
                                                                                                                   
;Castle Entry Rooms (Yellow, White, Black)
EntryRoomOffsets:                                                                         
	 .byte $12,$1A,$1B                                                                                           
                                                                                                                   
;Castle Rooms (Yellow, White, Black)                                                                               
CastleRoomOffsets:
	 .byte $11,$0F,$10                                                                                           
                                                                                                                   
                                                                                                                   
;Deal With Magnet.                                                                                                 
Mag_1: 
	 LDA    $B5                 ;Get Magnet's Y Coordinate.                                                ;3    
       SEC                                                                                                   ;2    
       SBC    #$08                ;Adjust to it's "Poles".                                                   ;2    
       STA    $B5                                                                                            ;3    
       LDA    #$00                ;Con Difficulty!                                                           ;2    
       STA    $D6                                                                                            ;3    
       LDA    #<MagnetMatrix                ;Set Low Address of Object Store.                                          ;2    
       STA    $D2                                                                                            ;3    
       LDA    #>MagnetMatrix                ;Set High Address of Object Store.                                         ;2    
       STA    $D3                                                                                            ;3    
       JSR    GetLinkedObject     ;Get Liked Object and Set Movement.                                        ;6    
       LDA    $9B                 ;Get Movement.                                                             ;3    
       BEQ    Mag_2               ;If None, then Forget It.                                                  ;2    
                                                                                                                   
       LDY    #$01                ;Set Delta to One.                                                         ;2    
       JSR    MoveGroundObject     ;Move Object.                                                              ;6    
                                                                                                                   
Mag_2: 
	 LDA    $B5                 ;Reset the Magnet's                                                        ;3    
       CLC                        ;      Y Coordinate.                                                       ;2    
       ADC    #$08                                                                                           ;2    
       STA    $B5                                                                                            ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
;Magnet Object Matrix.                                                                                             
MagnetMatrix:
       .byte $BF,$B3                  ;Yellow Key, Magnet                                                          
       .byte $C2,$B3                  ;White Key, Magnet                                                           
       .byte $C5,$B3                  ;Black Key, Magnet                                                           
       .byte $B6,$B3                  ;Sword, Magnet                                                               
       .byte $BC,$B3                  ;Bridge, Magnet                                                              
       .byte $B9,$B3                  ;Chalise, Magnet                                                             
       .byte $00                                                                                                   
                                                                                                                   
                                                                                                                   
                                                                                                                   
;Deal with Invisible Surround Moving.                                                                              
Surround: 
	 LDA    $8A                 ;Get the Current Room.                                                     ;3    
       JSR    RoomNumToAddress    ;Convert it to an Address.                                                 ;6    
       LDY    #$02                                                                                           ;2    
       LDA    ($93),Y             ;Get the Room's Color.                                                     ;5    
       CMP    #$08                ;Is it Invisible?                                                          ;2    
       BEQ    Surround_2          ;If So Branch.                                                             ;2    
                                                                                                                   
       LDA    #$00                ;If not, signal the                                                        ;2    
       STA    $DB                 ;      Invisible surround not                                              ;3    
       JMP    Surround_4          ;      Wanted.                                                             ;3    

Surround_2:                                                                                                                   
       LDA    $8A                 ;Get the Current Room.                                                     ;3    
       STA    $D9                 ;And store as the Invisible Surrounds.                                     ;3    
       LDA    $8B                 ;Get the Ball's X Coordinate.                                              ;3    
       SEC                                                                                                   ;2    
       SBC    #$0E                ;Adjust for Surround,                                                      ;2    
       STA    $DA                 ;      and store as surround's X coordinate.                               ;3    
       LDA    $8C                 ;Get the Ball's Y Coordinate.                                              ;3    
       CLC                                                                                                   ;2    
       ADC    #$0E                ;Adjust for Surround.                                                      ;2    
       STA    $DB                 ;      and store as surround's Y coordinate.                               ;3    
       LDA    $DA                 ;Get the Surround's X cordinate.                                           ;3    
       CMP    #$F0                ;Is it close to the right edge?                                            ;2    
       BCC    Surround_3          ;Branch if not.                                                            ;2    
                                                                                                                   
       LDA    #$01                ;Flick surround to the                                                     ;2    
       STA    $DA                 ;      otherside of the screen.                                            ;3    
       JMP    Surround_4                                                                                     ;3    

Surround_3:                                                                                                                   
       CMP    #$82                ;???                                                                       ;2    
       BCC    Surround_4          ;???                                                                       ;2    
                                                                                                                   
       LDA    #$81                ;???                                                                       ;2    
       STA    $DA                 ;???                                                                       ;3    
Surround_4:
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Make A Noise.                                                                                                     
MakeSound:
	 LDA    $DF                 ;Check Not Count.                                                          ;3    
       BNE    MakeSound_2         ;Branch if Noise to be made.                                               ;2    
                                                                                                                   
       STA    AUDV0               ;Turn off the Volume.                                                      ;3    
       STA    AUDV1                                                                                          ;3    
       RTS                                                                                                   ;6    

MakeSound_2:                                                                                                                   
       DEC    $DF                 ;Goto the Next Note.                                                       ;5    
       LDA    $E0                 ;Get the Noise Type.                                                       ;3    
       BEQ    NoiseGameOver       ;Game Over                                                                 ;2    
                                                                                                                   
       CMP    #$01                ;Roar                                                                      ;2    
       BEQ    NoiseRoar                                                                                      ;2    
                                                                                                                   
       CMP    #$02                ;Man Eaten.                                                                ;2    
       BEQ    EatenNoise                                                                                     ;2    
                                                                                                                   
       CMP    #$03                ;Dying Dragon.                                                             ;2    
       BEQ    DragDieNoise                                                                                   ;2    
                                                                                                                   
       CMP    #$04                ;Dropping Object.                                                          ;2    
       BEQ    NoiseDropObject                                                                                          ;2    
                                                                                                                   
       CMP    #$05                ;Picking up Object.                                                        ;2    
       BEQ    NoiseGetObject                                                                                          ;2    
                                                                                                                   
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Noise 0 : Game Over                                                                                               
NoiseGameOver:
       LDA    $DF                                                                                            ;3    
       STA    COLUPF              ;Color-Luminance Playfield.                                                ;3    
       STA    AUDC0               ;Audio-Control 00                                                          ;3    
       LSR                                                                                                   ;2    
       STA    AUDV0               ;Audio-Volume 00                                                           ;3    
       LSR                                                                                                   ;2    
       LSR                                                                                                   ;2    
       STA    AUDF0               ;Audio-Frequency 00                                                        ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
;Noise 1 : Roar                                                                                                    
NoiseRoar:
       LDA    $DF                 ;Get noise count.                                                          ;3    
       LSR                                                                                                   ;2    
       LDA    #$03                ;If it was even then                                                       ;2    
       BCS    SetVolume           ;Branch.                                                                   ;2    
                                                                                                                   
       LDA    #$08                ;Get a differnt audio control value.                                       ;2    

SetVolume:                                                                                                                   
       STA    AUDC0               ;Set Audio Control 00.                                                     ;3    
       LDA    $DF                 ;Set the Volume to the Noise Count.                                        ;3    
       STA    AUDV0                                                                                          ;3    
       LSR                        ;Divide by Four.                                                           ;2    
       LSR                                                                                                   ;2    
       CLC                                                                                                   ;2    
       ADC    #$1C                ;Set the Frequency.                                                        ;2    
       STA    AUDF0                                                                                          ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Noise 2 : Man Eaten                                                                                               
EatenNoise:
       LDA    #$06                                                                                           ;2    
       STA    AUDC0               ;Audio-Control 00                                                          ;3    
       LDA    $DF                                                                                            ;3    
       EOR    #$0F                                                                                           ;2    
       STA    AUDF0               ;Audio-Frequency 00                                                        ;3    
       LDA    $DF                                                                                            ;3    
       LSR                                                                                                   ;2    
       CLC                                                                                                   ;2    
       ADC    #$08                                                                                           ;2    
       STA    AUDV0               ;Audio-Volume 00                                                           ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Noise 3 : Dyning Dragon                                                                                           
DragDieNoise:
       LDA    #$04                ;Set the Audio Control                                                     ;2    
       STA    AUDC0                                                                                          ;3    
       LDA    $DF                 ;Put the Note Count In                                                     ;3    
       STA    AUDV0               ;      the Volume.                                                         ;3    
       EOR    #$1F                                                                                           ;2    
       STA    AUDF0               ;Flip the Count as store                                                   ;3    
       RTS                        ;      as the frequency.                                                   ;6    
                                                                                                                   
                                                                                                                   
;Noise 4 : Dropping Object.                                                                                        
NoiseDropObject:
       LDA    $DF                 ;Get Not Count                                                             ;3    
       EOR    #$03                ;Reverse it as noise does up.                                              ;2    
NoiseDropObject_2:
       STA    AUDF0               ;Store in Frequency for Channel 00.                                        ;3    
       LDA    #$05                                                                                           ;2    
       STA    AUDV0               ;Set Volume on Channel 00.                                                 ;3    
       LDA    #$06                                                                                           ;2    
       STA    AUDC0               ;Set a Noise on Channel 00.                                                ;3    
       RTS                                                                                                   ;6    
                                                                                                                   
                                                                                                                   
;Noise 5 : Picking up an Object.                                                                                   
NoiseGetObject:
       LDA    $DF                 ;Get Not Count.                                                            ;3    
       JMP    NoiseDropObject_2   ;      and Make Same noise as Drop.                                        ;3    
                                                                                                                   
                                                                                                                   
;Left of Name Room                                                                                                 
LeftOfName:
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRRRRRR                                  
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           

;Below Yellow Castle                                                                                               
BelowYellowCastle:
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRRRRRR   **Line Shared With Above Room ----^                               
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRRRRRR                                  
                                                                                                                   
                                                                                                                   
;Side Corridor                                                                                                     
SideCorridor:
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $00,$00,$00                                                                                           
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
                                                                                                                   
                                                                                                                   
;Number Room Definition                                                                                            
NumberRoom:
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
                                                                                                                   
                                                                                                                   
;Object #1 States (Portcullis)                                                                                     
PortStates:
       .byte $04,<GfxPort07,>GfxPort07          ;State 04 at FB24 -Open                                                        
       .byte $08,<GfxPort06,>GfxPort06          ;State 08 at FB22                                                              
       .byte $0C,<GfxPort05,>GfxPort05          ;State 0C at FB20                                                              
       .byte $10,<GfxPort04,>GfxPort04          ;State 10 at FB1E                                                              
       .byte $14,<GfxPort03,>GfxPort03          ;State 14 at FB1C                                                             
       .byte $18,<GfxPort02,>GfxPort02          ;State 18 at FB1A                                                              
LFB03: .byte $1C,<GfxPort01,>GfxPort01          ;State 1C at FB18 -Closed                                               
       .byte $20,<GfxPort02,>GfxPort02          ;State 20 at FB1A                                                              
       .byte $24,<GfxPort03,>GfxPort03          ;State 24 at FB1C                                                              
       .byte $28,<GfxPort04,>GfxPort04          ;State 28 at FB1E                                                              
       .byte $2C,<GfxPort05,>GfxPort05          ;State 2C at FB20                                                              
       .byte $30,<GfxPort06,>GfxPort06          ;State 30 at FB22                                                              
LFB15: .byte $FF,<GfxPort07,>GfxPort07          ;State FF at FB24 -Open                                                        
                                                                                                                   
                                                                                                                   
;Object #1 States 940FF (Graphic)                                                                                  
GfxPort01:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort02:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort03:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort04:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort05:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort06:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort07:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort08:
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $AA                  ;X X X X                                                                   
GfxPort09:
       .byte $00                                                                                                   
                                                                                                                   
;Two Exit Room                                                                                                     
TwoExitRoom:
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
                                                                                                                   
                                                                                                                   
;Top of Blue Maze                                                                                                  
BlueMazeTop:
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$0C,$0C          ;        XX    XX        RR    RR                                              
       .byte $F0,$0C,$3C          ;XXXX    XX    XXXX    RRRR    RR    RRRR                                      
       .byte $F0,$0C,$00          ;XXXX    XX                    RR    RRRR                                      
       .byte $F0,$FF,$3F          ;XXXXXXXXXXXXXXXXXX    RRRRRRRRRRRRRRRRRR                                      
       .byte $00,$30,$30          ;      XX        XX    RR        RR                                            
       .byte $F0,$33,$3F          ;XXXX  XX  XXXXXXXX    RRRRRRRR  RR  RRRR                                      
                                                                                                                 
;Blue Maze #1                                                                                                      
BlueMaze1:
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $F0,$FC,$FF          ;XXXXXXXXXX  XXXXXXXXRRRRRRRR  RRRRRRRRRR                                      
       .byte $F0,$00,$C0          ;XXXX              XXRR              RRRR                                      
       .byte $F0,$3F,$CF          ;XXXX  XXXXXXXXXX  XXRR  RRRRRRRRRR  RRRR                                      
       .byte $00,$30,$CC          ;      XX      XX  XXRR  RR      RR                                            
       .byte $F0,$F3,$CC          ;XXXXXXXX  XX  XX  XXRR  RR  RR  RRRRRRRR                                      
                                                                                                                   
;Bottom of Blue Maze                                                                                               
BlueMazeBottom:
       .byte $F0,$F3,$0C          ;XXXXXXXX  XX  XX        RR  RR  RRRRRRRR                                      
       .byte $00,$30,$0C          ;      XX      XX        RR      RR                                           
       .byte $F0,$3F,$0F          ;XXXX  XXXXXXXXXX        RRRRRRRRRR  RRRR                                      
       .byte $F0,$00,$00          ;XXXX                                RRRR                                      
       .byte $F0,$F0,$00          ;XXXXXXXX                        RRRRRRRR                                      
       .byte $00,$30,$00          ;      XX                        RR                                            
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Center of Blue Maze                                                                                               
BlueMazeCenter:
       .byte $F0,$33,$3F          ;XXXX  XX  XXXXXXXX    RRRRRRRR  RR  RRRR                                      
       .byte $00,$30,$3C          ;      XX      XXXX    RRRR      RR                                            
       .byte $F0,$FF,$3C          ;XXXXXXXXXXXX  XXXX    RRRR  RRRRRRRRRRRR                                      
       .byte $00,$03,$3C          ;          XX  XXXX    RRRR  RR                                                
       .byte $F0,$33,$3C          ;XXXX  XX  XX  XXXX    RRRR  RR  RR  RRRR                                      
       .byte $00,$33,$0C          ;      XX  XX  XX        RR  RR  RR                                            
       .byte $F0,$F3,$0C          ;XXXXXXXX  XX  XX        RR  RR  RRRRRRRR                                      
                                                                                                                   
;Blue Maze Entry                                                                                                   
BlueMazeEntry:
       .byte $F0,$F3,$CC          ;XXXXXXXX  XX  XX  XXRR  RR  RR  RRRRRRRR                                      
       .byte $00,$33,$0C          ;      XX  XX  XX        RR  RR  RR                                            
       .byte $F0,$33,$FC          ;XXXX  XX  XX  XXXXXXRRRRRR  RR  RR  RRRR                                      
       .byte $00,$33,$00          ;      XX  XX                RR  RR                                            
       .byte $F0,$F3,$FF          ;XXXXXXXX  XXXXXXXXXXRRRRRRRRRR  RRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Maze Middle                                                                                                       
MazeMiddle:
       .byte $F0,$FF,$CC          ;XXXXXXXXXXXX  XX  XXRR  RR  RRRRRRRRRRRR                                      
       .byte $00,$00,$CC          ;              XX  XXRR  RR                                                    
       .byte $F0,$03,$CF          ;XXXX      XXXXXX  XXRR  RRRRRR      RRRR                                      
       .byte $00,$03,$00          ;          XX                RR                                                
       .byte $F0,$F3,$FC          ;XXXXXXXX  XX  XXXXXXRRRRRR  RR  RRRRRRRR                                     
       .byte $00,$33,$0C          ;      XX  XX  XX        RR  RR  RR                                            

;Maze Side 
MazeSide:                                                                                                        
       .byte $F0,$33,$CC          ;XXXX  XX  XX  XX  XXRR  RR  RR  RR  RRRR     **Line Shared With Above Room ----^                                         
       .byte $00,$30,$CC          ;      XX      XX  XXRR  RR      RR                                            
       .byte $00,$3F,$CF          ;      XXXXXX  XX  XXRR  RR  RRRRRR                                            
       .byte $00,$00,$C0          ;                  XXRR                                                        
       .byte $00,$3F,$C3          ;      XXXXXXXX    XXRR    RRRRRRRR                                            
       .byte $00,$30,$C0          ;      XX          XXRR          RR                                            
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Maze Entry                                                                                                        
MazeEntry:
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$30,$00          ;      XX                        RR                                            
       .byte $F0,$30,$FF          ;XXXX  XX    XXXXXXXXRRRRRRRRR   RR  RRRR                                      
       .byte $00,$30,$C0          ;      XX          XXRR          RR                                            
       .byte $F0,$F3,$C0          ;XXXXXXXX  XX      XXRR      RR  RRRRRRRR                                      
       .byte $00,$03,$C0          ;          XX      XXRR      RR                                                
       .byte $F0,$FF,$CC          ;XXXXXXXXXXXX  XX  XXRR  RR  RRRRRRRRRRRR                                      
                                                                                                                   
;Castle Definition                                                                                                 
CastleDef:
       .byte $F0,$FE,$15          ;XXXXXXXXXXX X X X      R R R RRRRRRRRRRR                                      
       .byte $30,$03,$1F          ;XX        XXXXXXX      RRRRRRR        RR                                      
       .byte $30,$03,$FF          ;XX        XXXXXXXXXXRRRRRRRRRR        RR                                      
       .byte $30,$00,$FF          ;XX          XXXXXXXXRRRRRRRR          RR                                      
       .byte $30,$00,$3F          ;XX          XXXXXX    RRRRRR          RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXX            RRRRRRRRRRRRRR                                      
                                                                                                                   
;Object Data                                                                                                       
;Offset 0 : Room number of object.                                                                                 
;Offset 1 : X Coordinate of object.                                                                                
;Offset 2 : Y Coordinate of object.                                                                                
                                                                                                                   
;Object #1 : Portcullis                                                                                            
PortInfo1:
       .byte $11,$4D,$31          ;Room 11, (4D, 31)                                                             
;Object #2 : Portcullis                                                                                          
PortInfo2:
       .byte $0F,$4D,$31          ;Room 0F, (4D, 31)                                                             
;Object #3 : Portcullis                                                                                           
PortInfo3:
       .byte $10,$4D,$31          ;Room 10, (4D, 31                                                              
                                                                                                                   
;Object #0 : State                                                                                                 
SurroundCurr:
       .byte $00                                                                                                   
                                                                                                                   
;Object #1 : State List                                                                                            
SurroundStates:
       .byte $FF,<GfxSurround,>GfxSurround          ;State FF as FC05                                                              
                                                                                                                   
;Object #1 : Graphic                                                                                               
GfxSurround:
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $00                                                                                                   
                                                                                                                   
;Red Maze #1                                                                                                       
RedMaze1:
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$0C          ;                  XX        RR                                                
       .byte $F0,$FF,$0C          ;XXXXXXXXXXXX  XX        RR  RRRRRRRRRRRR                                      
       .byte $F0,$03,$CC          ;XXXX      XX  XX  XXRR  RR  RR      RRRR                                      

;Bottom of Red Maze                                                                                                
RedMazeBottom:
       .byte $F0,$33,$CF          ;XXXX  XX  XXXXXX  XXRR  RRRRRR  RR  RRRR     **Line Shared With Above Room ----^  
       .byte $F0,$30,$00          ;XXXX  XX                        RR  RRRR                                      
       .byte $F0,$33,$FF          ;XXXX  XX  XXXXXXXXXXRRRRRRRRRR  RR  RRRR                                      
       .byte $00,$33,$00          ;      XX  XX                RR  RR  RRRR                                      
       .byte $F0,$FF,$00          ;XXXXXXXXXXXX                RRRRRRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Top of Red Maze                                                                                                   
RedMazeTop:
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$C0          ;                  XXRR                                                        
       .byte $F0,$FF,$CF          ;XXXXXXXXXXXXXXXX  XXRR  RRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$CC          ;              XX  XXRR  RR                                                    
       .byte $F0,$33,$FF          ;XXXX  XX  XXXXXXXXXXRRRRRRRRRR  RR  RRRR                                      
       .byte $F0,$33,$00          ;XXXX  XX  XX                RR  RR  RRRR                                      

;White Castle Entry                                                                                                
WhiteCastleEntry:
       .byte $F0,$3F,$0C          ;XXXX  XXXXXX  XX        RR  RRRRRR  RRRR     **Line Shared With Above Room ----^  
       .byte $F0,$00,$0C          ;XXXX          XX        RR          RRRR                                      
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$30,$00          ;      XX                        RR                                            
       .byte $F0,$30,$00          ;XXXX  XX                        RR  RRRR                                      
       .byte $00,$30,$00          ;      XX                        RR                                            
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Top Entry Room  
TopEntryRoom:                                                                                                  
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $30,$00,$00          ;XX                                    RR                                      
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXRRRRRRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Black Maze #1                                                                                                     
BlackMaze1:
       .byte $F0,$F0,$FF          ;XXXXXXXX    XXXXXXXXRRRRRRRR    RRRRRRRR                                      
       .byte $00,$00,$03          ;            XX            RR                                                  
       .byte $F0,$FF,$03          ;XXXXXXXXXXXXXX            RRRRRRRRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $30,$3F,$FF          ;XX    XXXXXXXXXXXXXXRRRRRRRRRRRRRR    RR                                      
       .byte $00,$30,$00          ;      XX                        RR                                            

;Black Maze #3                                                                                                     
BlackMaze3:
       .byte $F0,$F0,$FF          ;XXXXXXXX    XXXXXXXXRRRRRRRR    RRRRRRRR    **Line Shared With Above Room ----^ (Mirrored Not Reversed)  
       .byte $30,$00,$00          ;XX                  MM                                                        
       .byte $30,$3F,$FF          ;XX    XXXXXXXXXXXXXXMM    MMMMMMMMMMMMMM                                      
       .byte $00,$30,$00          ;      XX                  MM                                                  
       .byte $F0,$F0,$FF          ;XXXXXXXX    XXXXXXXXMMMMMMMM    MMMMMMMM                                      
       .byte $30,$00,$03          ;XX          XX      MM          MM                                            
       .byte $F0,$F0,$FF          ;XXXXXXXX    XXXXXXXXMMMMMMMM    MMMMMMMM                                      
                                                                                                                   
;Black Maze #2                                                                                                     
BlackMaze2:
       .byte $F0,$FF,$FF          ;XXXXXXXXXXXXXXXXXXXXMMMMMMMMMMMMMMMMMMMM                                      
       .byte $00,$00,$C0          ;                  XX                  MM                                      
       .byte $F0,$FF,$CF          ;XXXXXXXXXXXXXXXX  XXMMMMMMMMMMMMMMMM  MM                                      
       .byte $00,$00,$0C          ;                  XX                  MM                                      
       .byte $F0,$0F,$FF          ;XXXX    XXXXXXXXXXXXMMMM    MMMMMMMMMMMM                                      
       .byte $00,$0F,$C0          ;        XXXX      XX        MMMM      MM                                      

;Black Maze Entry                                                                                                  
BlackMazeEntry:
       .byte $30,$CF,$CC          ;XX  XX  XXXX  XX  XXMM  MM  MMMM  MM  MM  **Line Shared With Above Room ----^ (Reversed Not Mirrored)                                    
       .byte $00,$C0,$CC          ;        XX        XX  XXRR  RR        RR                                      
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
       .byte $00,$00,$00          ;                                                                              
       .byte $F0,$FF,$0F          ;XXXXXXXXXXXXXXXX        RRRRRRRRRRRRRRRR                                      
                                                                                                                   
;Objtect #0A : State  
BridgeCurr:                                                                                             
       .byte $00                                                                                                   
                                                                                                                   
;Object #0A : List of States  
BridgeStates:                                                                                     
       .byte $FF,<GfxBridge,>GfxBridge          ;State FF at &FCDB                                                             
                                                                                                                   
;Object #0A : State FF : Graphic                                                                                   
GfxBridge:
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $00                                                                                                   
                                                                                                                   
;Object #5 State #1 Graphic :'1'                                                                                   
GfxNum1:
       .byte $04                  ; X                                                                        
       .byte $0C                  ;XX                                                                        
       .byte $04                  ; X                                                                        
       .byte $04                  ; X                                                                        
       .byte $04                  ; X                                                                        
       .byte $04                  ; X                                                                        
       .byte $0E                  ;XXX                                                                       
       .byte $00                                                                                                   
                                                                                                                   
;Object #0B : State                                                                                                
KeyCurr:
       .byte $00                                                                                                   
                                                                                                                   
;Object #0B : List of States                                                                                       
KeyStates:
       .byte $FF,<GfxKey,>GfxKey                                                                                           
                                                                                                                   
;Object #0B : State FF : Graphic                                                                                   
GfxKey:
       .byte $07                  ;     XXX                                                                  
       .byte $FD                  ;XXXXXX X                                                                  
       .byte $A7                  ;X X  XXX                                                                  
       .byte $00                                                                                                   
                                                                                                                   
;Object #5 State #2 Grphic : '2'                                                                                   
GfxNum2:
       .byte $0E                  ; XXX                                                                      
       .byte $11                  ;X   X                                                                     
       .byte $01                  ;    X                                                                     
       .byte $02                  ;   X                                                                      
       .byte $04                  ;  X                                                                       
       .byte $08                  ; X                                                                        
       .byte $1F                  ;XXXXX                                                                     
       .byte $00                                                                                                   
                                                                                                                   
;Object #5 State #3 Graphic :'3'                                                                                   
GfxNum3:
       .byte $0E                  ; XXX                                                                      
       .byte $11                  ;X   X                                                                     
       .byte $01                  ;    X                                                                     
       .byte $06                  ;  XX                                                                      
       .byte $01                  ;    X                                                                     
       .byte $11                  ;X   X                                                                     
       .byte $0E                  ; XXX                                                                      
       .byte $00                                                                                                   
                                                                                                                   
                                                                                                                   
;Object #0E : List of States   
BatStates:                                                                                    
       .byte $03,<GfxBat1,>GfxBat1          ;State 03 at &FD1A                                                             
LFD17: .byte $FF,<GfxBat2,>GfxBat2          ;State FF as &FD22                                                             
                                                                                                                   
;Object #0E : State 03 : Graphic                                                                                   
GfxBat1:
       .byte $81                  ;X      X                                                                  
       .byte $81                  ;X      X                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $5A                  ; X XX X                                                                   
       .byte $66                  ; XX  XX                                                                   
       .byte $00                                                                                                   
                                                                                                                   
;Object #0E : State FF : Graphic                                                                                   
GfxBat2:
       .byte $01                  ;       X                                                                  
       .byte $80                  ;X                                                                         
       .byte $01                  ;       X                                                                  
       .byte $80                  ;X                                                                         
       .byte $3C                  ;  XXXX                                                                    
       .byte $5A                  ; X XX X                                                                   
       .byte $66                  ; XX  XX                                                                   
       .byte $C3                  ;XX    XX                                                                  
       .byte $81                  ;X      X                                                                  
       .byte $81                  ;X      X                                                                  
       .byte $81                  ;X      X                                                                  
       .byte $00                                                                                                   
                                                                                                                   
;Object #6 : States
DragonStates:                                                                                                
       .byte $00,<GfxDrag0,>GfxDrag0          ;State 00 at &FD3A                                                             
LFD31: .byte $01,<GfxDrag2,>GfxDrag2          ;State 01 at &FD66                                                             
LFD34: .byte $02,<GfxDrag0,>GfxDrag0          ;State 02 at &FD3A                                                             
LFD37: .byte $FF,<GfxDrag1,>GfxDrag1          ;State FF at &FD4F                                                             
                                                                                                                   
;Object #6 : State #00 : Graphic                                                                                   
GfxDrag0:
       .byte $06                  ;     XX                                                                   
       .byte $0F                  ;    XXXX                                                                  
       .byte $F3                  ;XXXX  XX                                                                  
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $0E                  ;    XXX                                                                   
       .byte $04                  ;     X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $1E                  ;   XXXX                                                                   
       .byte $3F                  ;  XXXXXX                                                                  
       .byte $7F                  ; XXXXXXX                                                                  
       .byte $E3                  ;XXX   XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C7                  ;XX   XXX                                                                  
       .byte $FF                  ;XXXXXXXX                                                                  
       .byte $3C                  ;  XXXX                                                                    
       .byte $08                  ;    X                                                                     
       .byte $8F                  ;X   XXXX                                                                  
       .byte $E1                  ;XXX    X                                                                  
       .byte $3F                  ;  XXXXXX                                                                  
       .byte $00                                                                                                   
                                                                                                                   
;Object 6 : State FF : Graphic                                                                                     
GfxDrag1:
       .byte $80                  ;X                                                                         
       .byte $40                  ; X                                                                        
       .byte $26                  ;  X  XX                                                                   
       .byte $1F                  ;   XXXXX                                                                  
       .byte $0B                  ;    X XX                                                                  
       .byte $0E                  ;    XXX                                                                   
       .byte $1E                  ;   XXXX                                                                   
       .byte $24                  ;  X  X                                                                    
       .byte $44                  ; X   X                                                                    
       .byte $8E                  ;X   XXX                                                                   
       .byte $1E                  ;   XXXX                                                                  
       .byte $3F                  ;  XXXXXX                                                                  
       .byte $7F                  ; XXXXXXX                                                                  
       .byte $7F                  ; XXXXXXX                                                                  
       .byte $7F                  ; XXXXXXX                                                                  
       .byte $7F                  ; XXXXXXX                                                                  
       .byte $3E                  ;  XXXXX                                                                   
       .byte $1C                  ;   XXX                                                                    
       .byte $08                  ;    X                                                                     
       .byte $F8                  ;XXXXX                                                                     
       .byte $80                  ;X                                                                         
       .byte $E0                  ;XXX                                                                       
       .byte $00                                                                                                   
                                                                                                                   
;Object 6 : State 02 : Graphic                                                                                     
GfxDrag2:
       .byte $0C                  ;    XX                                                                    
       .byte $0C                  ;    XX                                                                    
       .byte $0C                  ;    XX                                                                    
       .byte $0E                  ;    XXX                                                                   
       .byte $1B                  ;   XX X                                                                   
       .byte $7F                  ; XXXXXXX                                                                  
       .byte $CE                  ;XX  XXX                                                                   
       .byte $80                  ;X                                                                         
       .byte $FC                  ;XXXXXX                                                                    
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $FE                  ;XXXXXXX                                                                   
       .byte $7E                  ; XXXXXX                                                                   
       .byte $78                  ; XXXX                                                                     
       .byte $20                  ;  X                                                                       
       .byte $6E                  ; XX XXX                                                                   
       .byte $42                  ; X    X                                                                   
       .byte $7E                  ; XXXXXX                                                                   
       .byte $00                                                                                                   
                                                                                                                   
;Object #9 : Current State   
SwordCurr:                                                                                              
       .byte $00                                                                                                   
                                                                                                                   
;Object #9 : List of States                                                                                        
SwordStates:
       .byte $FF,<GfxSword,>GfxSword          ;State FF at &FD7C                                                             
                                                                                                                   
;Object #9 : State FF : Graphics                                                                                   
GfxSword:
       .byte $20                  ;  X                                                                       
       .byte $40                  ; X                                                                        
       .byte $FF                  ;XXXXXXXX     
       .byte $40                  ; X                                                                        
       .byte $20                  ;  X                                                                       
       .byte $00                                                                                                   
                                                                                                                   
;Object #0F : State  
DotCurr:                                                                                              
       .byte $00                                                                                                   
                                                                                                                   
;Object #0F : List of States                                                                                       
DotStates:
       .byte $FF,<GfxDot,>GfxDot          ;State FF at FD86                                                              
                                                                                                                   
;Object #0F : State FF : Graphic                                                                                   
GfxDot:
       .byte $80                  ;X                                                                         
       .byte $00                                                                                                   
                                                                                                                   
;Object #4 : State FF : Graphic                                                                                    
GfxAuthor:
       .byte $F0                  ;XXXX                                                                      
       .byte $80                  ;X                                                                         
       .byte $80                  ;X                                                                         
       .byte $80                  ;X                                                                         
       .byte $F4                  ;XXXX X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $87                  ;X    XXX                                                                  
       .byte $E5                  ;XXX  X X                                                                  
       .byte $87                  ;X    XXX                                                                  
       .byte $80                  ;X                                                                         
       .byte $05                  ;     X X                                                                  
       .byte $E5                  ;XXX  X X                                                                 
       .byte $A7                  ;X X  XXX                                                                  
       .byte $E1                  ;XXX    X                                                                  
       .byte $87                  ;X    XXX                                                                  
       .byte $E0                  ;XXX                                                                       
       .byte $01                  ;       X                                                                  
       .byte $E0                  ;XXX                                                                       
       .byte $A0                  ;X X                                                                       
       .byte $F0                  ;XXXX                                                                      
       .byte $01                  ;       X                                                                  
       .byte $40                  ; X                                                                        
       .byte $E0                  ;XXX                                                                       
       .byte $40                  ; X                                                                       
       .byte $40                  ; X                                                                        
       .byte $40                  ; X                                                                        
       .byte $01                  ;       X                                                                  
       .byte $E0                  ;XXX                                                                       
       .byte $A0                  ;X X                                                                       
       .byte $E0                  ;XXX                                                                       
       .byte $80                  ;X                                                                         
       .byte $E0                  ;XXX                                                                       
       .byte $01                  ;       X                                                                  
       .byte $20                  ;  X                                                                       
       .byte $20                  ;  X                                                                       
       .byte $E0                  ;XXX                                                                       
       .byte $A0                  ;X X                                                                       
       .byte $E0                  ;XXX                                                                       
       .byte $01                  ;       X                                                                  
       .byte $01                  ;       X                                                                  
       .byte $01                  ;       X                                                                  
       .byte $88                  ;   X   X                                                                  
       .byte $A8                  ;X X X                                                                     
       .byte $A8                  ;X X X                                                                     
       .byte $A8                  ;X X X                                                                     
       .byte $F8                  ;XXXXX                                                                     
       .byte $01                  ;       X                                                                  
       .byte $E0                  ;XXX                                                                       
       .byte $A0                  ;X X                                                                       
       .byte $F0                  ;XXXX                                                                      
       .byte $01                  ;       X                                                                  
       .byte $80                  ;X                                                                         
       .byte $E0                  ;XXX                                                                       
       .byte $8F                  ;X   XXXX                                                                 
       .byte $89                  ;X   X  X                                                                  
       .byte $0F                  ;    XXXX                                                                  
       .byte $8A                  ;X   X X                                                                   
       .byte $E9                  ;XXX X  X                                                                  
       .byte $80                  ;X                                                                         
       .byte $8E                  ;X   XXX                                                                   
       .byte $0A                  ;    X X                                                                   
       .byte $EE                  ;XXX XXX                                                                   
       .byte $A0                  ;X X                                                                      
       .byte $E8                  ;XXX X                                                                     
       .byte $88                  ;X   X                                                                     
       .byte $EE                  ;XXX XXX                                                                   
       .byte $0A                  ;    X X                                                                   
       .byte $8E                  ;X   XXX                                                                   
       .byte $E0                  ;XXX                                                                       
       .byte $A4                  ;X X  X                                                                    
       .byte $A4                  ;X X  X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $80                  ;X                                                                         
       .byte $08                  ;    X                                                                     
       .byte $0E                  ;    XXX                                                                   
       .byte $0A                  ;    X X                                                                   
       .byte $0A                  ;    X X                                                                   
       .byte $80                  ;X                                                                         
       .byte $0E                  ;    XXX                                                                   
       .byte $0A                  ;    X X                                                                   
       .byte $0E                  ;    XXX                                                                   
       .byte $08                  ;    X                                                                     
       .byte $0E                  ;    XXX                                                                   
       .byte $80                  ;X                                                                         
       .byte $04                  ;     X                                                                    
       .byte $0E                  ;    XXX                                                                   
       .byte $04                  ;     X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $80                  ;X                                                                         
       .byte $04                  ;     X                                                                    
       .byte $0E                  ;    XXX                                                                   
       .byte $04                  ;     X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $04                  ;     X                                                                    
       .byte $00                                                                                                   
                                                                                                                   
;Object $4 : Author's Name   
AuthorInfo:                                                                                      
       .byte $1E,$50,$69          ;Room 1E, (50, 69)                                                             
                                                                                                                   
;Object #4 : Current State  
AuthorCurr:                                                                                       
       .byte $00                                                                                                   
                                                                                                                   
;Object #4 : States                                                                                                
AuthorStates:
       .byte $FF,<GfxAuthor,>GfxAuthor          ;State FF at &FD88                                                             
                                                                                                                   
;Object #10 : State                                                                                                
ChalliseCurr:
       .byte $00                                                                                                   
                                                                                                                   
;Object #10 : List of States                                                                                       
ChalliseStates:
       .byte $FF,<GfxChallise,>GfxChallise          ;State FF at &FDF3                                                            
                                                                                                                   
;Object #10 : State FF : Graphic                                                                                   
GfxChallise:
       .byte $81                  ;X      X                                                                  
       .byte $81                  ;X      X                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $7E                  ; XXXXXX                                                                   
       .byte $7E                  ; XXXXXX                                                                  
       .byte $3C                  ;  XXXX                                                                    
       .byte $18                  ;   XX                                                                     
       .byte $18                  ;   XX                                                                     
       .byte $7E                  ; XXXXXX                                                                   
       .byte $00                                                                                                   
                                                                                                                   
;Object #12 : State                                                                                                
NullCurr:
       .byte $00                                                                                                   
                                                                                                                   
;Object #12 : List of States                                                                                       
NullStates:
       .byte $FF,<GfxNull,>GfxNull                                                                                           
                                                                                                                   
;Object #12 " State FF : Graphic                                                                                   
GfxNull:
       .byte $00                                                                                                   
                                                                                                                   
;Object #5 Number.     
NumberInfo:                                                                                            
       .byte $00,$50,$40          ;#5 Number: Room 00, (50,40)                                                  
                                                                                                                   
;Object #5 States.                                                                                                 
NumberStates:
       .byte $01,<GfxNum1,>GfxNum1          ;State 1 as FCF4                                                               
LFE08: .byte $03,<GfxNum2,>GfxNum2          ;State 3 as FD04                                                               
LFE0B: .byte $FF,<GfxNum3,>GfxNum3          ;State FF as FD0C                                                              
                                                                                                                   
;Object #11 : State                                                                                                
MagnetCurr:
       .byte $00                                                                                                   
                                                                                                                   
;Object #11 : List of States                                                                                       
MagnetStates:
       .byte $FF,<GfxMagnet,>GfxMagnet          ;State FF at FE12                                                              
                                                                                                                   
;Object #11 : State FF : Graphic                                                                                   
GfxMagnet:
       .byte $3C                  ;  XXXX                                                                    
       .byte $7E                  ; XXXXXX                                                                   
       .byte $E7                  ;XXX  XXX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $C3                  ;XX    XX                                                                  
       .byte $00                                                                                                   
                                                                                                                   
                                                                                                                   
;Room Data                                                                                                         
;Offset 0 : Low byte foom graphics data.                                                                           
;Offset 1 : High byte room graphics data                                                                           
;Offset 2 : Color                                                                                                  
;Offset 3 : B&W Color                                                                                              
;Offset 4 : Bits 5-0 : Playfield Control                                                                           
;            Bit 6 : True if right thin wall wanted.                                                               
;            Bit 7 : True if left thin wall wanted.                                                                
;Offset 5 : Room Above                                                                                             
;Offset 6 : Room Left                                                                                              
;Offset 7 : Room Down                                                                                              
;Offset 8 : Room Right                                                                                             

RoomDataTable:                                                                                                                   
LFE1B:  .byte <NumberRoom,>NumberRoom,                $66,$0A,$21,$00,$00,$00,$00      ;00; 'Number Room.                          Purple           
LFE24:  .byte <BelowYellowCastle,>BelowYellowCastle,  $D8,$0A,$A1,$08,$02,$80,$03      ;01; (Top Acess) Reflected/8 Clock Ball                      
LFE2D:  .byte <BelowYellowCastle,>BelowYellowCastle,  $C8,$0A,$21,$11,$03,$83,$01      ;02; (Top Access)                         Green              
LFE36:  .byte <LeftOfName,>LeftOfName,                $E8,$0A,$61,$06,$01,$86,$02      ;03; Left of Name                                            
LFE3F:  .byte <BlueMazeTop,>BlueMazeTop,              $86,$0A,$21,$10,$05,$07,$06      ;04; Top of Blue Maze                         Blue           
LFE48:  .byte <BlueMaze1,>BlueMaze1,                  $86,$0A,$21,$1D,$06,$08,$04      ;05; Blue Maze #1                                Blue        
LFE51:  .byte <BlueMazeBottom,>BlueMazeBottom,        $86,$0A,$21,$07,$04,$03,$05      ;06; Bottom of Blue Maze                  Blue               
LFE5A:  .byte <BlueMazeCenter,>BlueMazeCenter,        $86,$0A,$21,$04,$08,$06,$08      ;07; Center of Blue Maze                  Blue               
LFE63:  .byte <BlueMazeEntry,>BlueMazeEntry,          $86,$0A,$21,$05,$07,$01,$07      ;08; Blue Maze Entry                        Blue             
LFE6C:  .byte <MazeMiddle,>MazeMiddle,                $08,$08,$25,$0A,$0A,$0B,$0A      ;09; Maze Middle                               Invisible     
LFE75:  .byte <MazeEntry,>MazeEntry,                  $08,$08,$25,$03,$09,$09,$09      ;0A; Maze Entry                              Invisible       
LFE7E:  .byte <MazeSide,>MazeSide,                    $08,$08,$25,$09,$0C,$1C,$0D      ;0B; Maze Side                              Invisible      Re
LFE87:  .byte <SideCorridor,>SideCorridor,            $98,$0A,$61,$1C,$0D,$1D,$0B      ;0C; (Side Corridor)                                         
LFE90:  .byte <SideCorridor,>SideCorridor,            $B8,$0A,$A1,$0F,$0B,$0E,$0C      ;0D; (Side Corridor)                                         
LFE99:  .byte <TopEntryRoom,>TopEntryRoom,            $A8,$0A,$21,$0D,$10,$0F,$10      ;0E; (Top Entry Room)                                        
LFEA2:  .byte <CastleDef,>CastleDef,                  $0C,$0C,$21,$0E,$0F,$0D,$0F      ;0F; White Castle                              White         
LFEAB:  .byte <CastleDef,>CastleDef,                  $00,$02,$21,$01,$1C,$04,$1C      ;10; Black Castle                              Black         
LFEB4:  .byte <CastleDef,>CastleDef,                  $1A,$0A,$21,$06,$03,$02,$01      ;11; Yellow Castle                        Yellow             
LFEBD:  .byte <NumberRoom,>NumberRoom,                $1A,$0A,$21,$12,$12,$12,$12      ;12; Yellow Castle Entry                   Yellow            
LFEC6:  .byte <BlackMaze1,>BlackMaze1,                $08,$08,$25,$15,$14,$15,$16      ;13; Black Maze #1                          Invisible      Re
LFECF:  .byte <BlackMaze2,>BlackMaze2,                $08,$08,$24,$16,$15,$16,$13      ;14; Black Maze #2                        Invisible      Dupl
LFED8:  .byte <BlackMaze3,>BlackMaze3,                $08,$08,$24,$13,$16,$13,$14      ;15; Black Maze #3                        Invisible      Dupl
LFEE1:  .byte <BlackMazeEntry,>BlackMazeEntry,        $08,$08,$25,$14,$13,$1B,$15      ;16; Black Maze Entry                        Invisible      R
LFEEA:  .byte <RedMaze1,>RedMaze1,                    $36,$0A,$21,$19,$18,$19,$18      ;17; Red Maze #1                              Red            
LFEF3:  .byte <RedMazeTop,>RedMazeTop,                $36,$0A,$21,$1A,$17,$1A,$17      ;18; Top of Red Maze                        Red              
LFEFC:  .byte <RedMazeBottom,>RedMazeBottom,          $36,$0A,$21,$17,$1A,$17,$1A      ;19; Bottom of Red Maze                        Red           
LFF05:  .byte <WhiteCastleEntry,>WhiteCastleEntry,    $36,$0A,$21,$18,$19,$18,$19      ;1A; White Castle Entry                        Red           
LFF0E:  .byte <TwoExitRoom,>TwoExitRoom,              $36,$0A,$21,$89,$89,$89,$89      ;1B; Black Castle Entry                        Red           
LFF17:  .byte <NumberRoom,>NumberRoom,                $66,$0A,$21,$1D,$07,$8C,$08      ;1C; Other Purple Room                         Purple        
LFF20:  .byte <TopEntryRoom,>TopEntryRoom,            $36,$0A,$21,$8F,$01,$10,$03      ;1D; (Top Entry Room)                        Red             
LFF29:  .byte <BelowYellowCastle,>BelowYellowCastle,  $66,$0A,$21,$06,$01,$06,$03      ;1E; Name Room                              Purple           
                                                                                                                   
                                                                                                                   
;Room differences for different levels (level 1,2,3)  
RoomDiffs:                                                             
LFF32: .byte $10,$0F,$0F            ;Down from Room 01                                                             
LFF35: .byte $05,$11,$11            ;Down from Room 02                                                             
LFF38: .byte $1D,$0A,$0A            ;Down from Room 03                                                             
LFF3B: .byte $1C,$16,$16            ;U/L/R/D from Room 1B (Black Castle Room)                                      
LFF3E: .byte $1B,$0C,$0C            ;Down from Room 1C                                                             
LFF41: .byte $03,$0C,$0C            ;Up from Room 1D (Top Entry Room)                                              
                                                                                                                   
;Objects                                                                                                           
;Offset 0 : Low byte object information (moveable stuff)                                                           
;Offset 1 : High byte object information (moveable stuff)                                                          
;Offset 2 : Low byte to object's current state                                                                     
;Offset 3 : High byte to object's current state                                                                    
;Offset 4 : Low byte list of states                                                                                
;Offset 5 : High byte list of states                                                                               
;Offset 6 : Colour                                                                                                 
;Offset 7 : Colour in B&W.                                                                                         
;Offset 8 : Size of object                                                                                         

Store1:                                                                                                                   
       .byte $D9                        ;0      ;#0 Invisible Surround Offsets..      00                           
Store2:
       .byte $00                        ;1                                                                         
Store3:
       .byte <SurroundCurr              ;2                                                                         
Store4:
       .byte >SurroundCurr              ;3                                                                         
Store5:
       .byte <SurroundStates            ;4                                                                         
Store6:
       .byte >SurroundStates            ;5                                                                         
Store7:
       .byte $28                        ;6                                                                         
Store8:
       .byte $0C                        ;7                                                                         
Store9:
       .byte $07                        ;8                                                                         
                                                                                                                   
LFF4D:       .byte <PortInfo1,>PortInfo1,    $C8,$00,                      <PortStates,>PortStates,          $00,$00,$00      ;#1 Portcullis #1       Black            09                  
LFF56:       .byte <PortInfo2,>PortInfo2,    $C9,$00,                      <PortStates,>PortStates,          $00,$00,$00      ;#2 Portcullis #2       Black            12             
LFF5F:       .byte <PortInfo3,>PortInfo3,    $CA,$00,                      <PortStates,>PortStates,          $00,$00,$00      ;#3 Portcullis #3       Black            1B             
LFF68:       .byte <AuthorInfo,>AuthorInfo,  <AuthorCurr,>AuthorCurr,      <AuthorStates,>AuthorStates,      $CB,$00,$00      ;#4 Name                Flash            24                
LFF71:       .byte <NumberInfo,>NumberInfo,  $DD,$00,                      <NumberStates,>NumberStates,      $C8,$00,$00      ;#5 Number              Green            2D              
LFF7A:       .byte $A4,$00,                  $A8,$00,                      <DragonStates,>DragonStates,      $36,$0E,$00      ;#6 Dragon #1           Red              36                   
LFF83:       .byte $A9,$00,                  $AD,$00,                      <DragonStates,>DragonStates,      $1A,$06,$00      ;#7 Dragon #2           Yellow           3F                      
LFF8C:       .byte $AE,$00,                  $B2,$00,                      <DragonStates,>DragonStates,      $C8,$00,$00      ;#8 Dragon #3           Green            48                 
LFF95:       .byte $B6,$00,                  <SwordCurr,>SwordCurr,        <SwordStates,>SwordStates,        $1A,$06,$00      ;#9 Sword               Yellow           51                    
LFF9E:       .byte $BC,$00,                  <BridgeCurr,>BridgeCurr,      <BridgeStates,>BridgeStates,      $66,$02,$07      ;#0A Bridge             Purple           5A                  
LFFA7:       .byte $BF,$00,                  <KeyCurr,>KeyCurr,            <KeyStates,>KeyStates,            $1A,$06,$00      ;#0B Key #01            Yellow           63                       
LFFB0:       .byte $C2,$00,                  <KeyCurr,>KeyCurr,            <KeyStates,>KeyStates,            $0E,$0E,$00      ;#0C Key #02            White            6C                  
LFFB9:       .byte $C5,$00,                  <KeyCurr,>KeyCurr,            <KeyStates,>KeyStates,            $00,$00,$00      ;#0D Key #03            Black            75                  
LFFC2:       .byte $CB,$00,                  $CF,$00,                      <BatStates,>BatStates,            $00,$00,$00      ;#0E Bat                Black            7E                
LFFCB:       .byte $A1,$00,                  <DotCurr,>DotCurr,            <DotStates,>DotStates,            $08,$08,$00      ;#0F Black Dot          Light Gray       87                 
LFFD4:       .byte $B9,$00,                  <ChalliseCurr,>ChalliseCurr,  <ChalliseStates,>ChalliseStates,  $CB,$06,$00      ;#10 Challise           Flash            90                 
LFFDD:       .byte $B3,$00,                  <MagnetCurr,>MagnetCurr,      <MagnetStates,>MagnetStates,      $00,$06,$00      ;#11 Magnet             Black            99             
LFFE6:       .byte $BC,$00,                  <NullCurr,>NullCurr,          <NullStates,>NullStates,          $00,$00,$00      ;#12 Null               Black            A2               
                                                                                                                   
;Not Used                                                                                                          
LFFEF: .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00                                                     
                                                                                                                   
;6502 Vectors (Not Used??                                                                                          
LFFFA:  .byte $00,$F0                                                                                               
LFFFC:  .byte $00,$F0                                                                                               
LFFFE:  .byte $00,$F0                                                                                               
