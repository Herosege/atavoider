	processor 6502
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"

        seg.u Variables
	org $80
Random		.byte		

HeightP0	equ 5
HeightP1	equ 8
XPosP0		.byte
YPosP0		.byte
XPosP1		.byte
YPosP1		.byte
InputCount	.byte
Temp		.byte
PFIndex		.byte
FieldCount	.byte

ObstacleClock	.byte
Score		.byte
CurDrawX	.byte
CurDrawY	.byte
MaxObsAmount 	.byte
ObstacleBMap	ds 5

P0Ptr		.word
P1Ptr		.word
PFPtr		.word

TmpPlayerY0	.byte
TmpPlayerY1	.byte
TmpPF0		.byte
TmpPF1		.byte
TmpPF2		.byte

NumBuffer	ds 10

;code
	seg Code
        org $f000
Start
	CLEAN_START
        ;Pointers
        lda INTIM
        sta Random
        lda #<Frame0P0
        sta P0Ptr
        lda #>Frame0P0
        sta P0Ptr+1
        lda #<Frame0P1
        sta P1Ptr
        lda #>Frame0P1
        sta P1Ptr+1
        lda #<FieldData
        sta PFPtr
        lda #>FieldData
        sta PFPtr+1
        
	lda #203
        sta YPosP0
        lda #76
        sta XPosP0
        lda #216
        sta YPosP1
        lda #71
        sta XPosP1
        lda #1 
        sta VDELP0
        lda #5
        sta NUSIZ1

NextFrame
	;reset
        lsr SWCHB
        bcc Start

	VERTICAL_SYNC
	
	TIMER_SETUP 37
        lda #$0
        sta COLUBK
        lda #$0f
        sta COLUPF
        lda #1
        sta CTRLPF
        lda #72
        sta PFIndex
        lda YPosP0
        sta TmpPlayerY0
        lda YPosP1
        sta TmpPlayerY1
               
        ;Position X 
        lda XPosP0 
        ldx #0
        jsr SetPositionX
        
        lda XPosP1 
        ldx #1
        jsr SetPositionX
        
        sta WSYNC
        sta HMOVE
        
      	lda Score
	ldx #0
	jsr GetNumData
        
        TIMER_WAIT
        lda #0
        sta VBLANK
       
	ldy #0
KernelScore
	sta WSYNC
        tya
        lsr
        tax
        lda NumBuffer,x
        sta PF1
        SLEEP 21
        lda #0
        sta PF1
	iny
        cpy #10
	bcc KernelScore
        
	
  
KernelMiddle
	jsr DrawSprites
	ldy PFIndex        
	beq SegEnd
        dey
        lda (PFPtr),y
        sty PFIndex
        sta TmpPF0
        
        jsr DrawSprites
        ldy PFIndex
	dey
        lda (PFPtr),y
        sty PFIndex
        sta TmpPF1
        
        jsr DrawSprites
        ldy PFIndex
	dey 
        lda (PFPtr),y
        sty PFIndex
        sta TmpPF2
        
        jsr DrawSprites
        lda TmpPF0
        sta PF0
        lda TmpPF1
        sta PF1
        lda TmpPF2
        sta PF2
        jmp KernelMiddle
SegEnd
        
        TIMER_SETUP 20
        lda #2
        sta VBLANK
        bit CXPPMM
	bpl Alive
       	jmp Start
Alive
	jsr GenObstacles
        jsr GetInput
        
        TIMER_WAIT
	
        ;End frame
        
        jmp NextFrame
        
;;;Subroutines, aka functions;;;

;Obstacles and score
GenObstacles subroutine
	lda Random
	lsr
	bcc .NoEor
	eor #$d4 ; #%11010100
.NoEor
	sta Random
	inc ObstacleClock
        lda #50
	cmp ObstacleClock
        bcs .EndObsGen
        lda #<Frame0P1
        sta P1Ptr
        lda #>Frame0P1
        sta P1Ptr+1
        lda #110
	cmp ObstacleClock
        bcs .EndObsGen
        lda #<Frame1P1
        sta P1Ptr
        lda #>Frame1P1
        sta P1Ptr+1
        lda #0 
        sta ObstacleClock
	inc Score
.EndObsGen
	rts


;Getting Number Data
GetNumData subroutine
	pha
        and #$0F
        sta Temp
        asl
        asl
        adc Temp
        tay
        lda #5
        sta Temp
.NumPart1
        lda NumData,y
        and #$0F
        sta NumBuffer,x
        iny
        inx
        dec Temp
        bne .NumPart1
        pla
        lsr
        lsr
        lsr
        lsr
        sta Temp
        asl
        asl
        adc Temp
        tay
        dex
        dex
        dex
        dex
        dex
        lda #5
        sta Temp
.NumPart2
        lda NumData,y
        and #$F0
        ora NumBuffer,x
        sta NumBuffer,x
        iny
        inx
        dec Temp
        bne .NumPart2
	rts
        

DrawSprites subroutine
	;Draw P0
	lda #HeightP0
        sec
        isb TmpPlayerY0
        bcs .DrawP0
        lda #0
.DrawP0
	tay
        lda (P0Ptr),y
        sta GRP0
        
        ;Draw P1
	lda #HeightP1
        sec
        isb TmpPlayerY1
        bcs .DrawP1
        lda #0
.DrawP1
	tay
        lda (P1Ptr),y

        ldx #$3f
        ldy #$46
        sta WSYNC
        sta GRP1
        stx COLUP0
        sty COLUP1
        
	rts
	
;XPos
SetPositionX subroutine
	sta WSYNC
        bit 0 ; 3 position skip
        sec
.DivLoop
	sbc #15
        bcs .DivLoop
        eor #7
        asl
        asl
        asl
        asl
        sta RESP0,x
        sta HMP0,x
        rts  
;Input      
GetInput subroutine
	;check if key is already pressed
        lda INPT4
        lda #$ff
        eor SWCHA
        beq .StartInput
        lda InputCount
        cmp #0
        bne .ButAlPressed
.StartInput
        lda #0
        sta InputCount
        ;Up - Down
	ldx YPosP0
	lda #$10
        bit SWCHA
        bne .SkipUp
        cpx #220
        bcs .SkipUp
       	clc
        lda #12
        adc YPosP0
        tax
        inc InputCount
.SkipUp
        lda #$20
        bit SWCHA
        bne .SkipDown
        cpx #180
        bcc .SkipDown
        clc
        lda #-12
        adc YPosP0
        tax 
        inc InputCount
.SkipDown
	stx YPosP0
        ;Left - Right
        ldx XPosP0
        lda #$40
        bit SWCHA
        bne .SkipLeft
        cpx #25
        bcc .SkipLeft
        clc
        lda #-20
        adc XPosP0
        tax 
        inc InputCount
.SkipLeft
        lda #$80
        bit SWCHA
        bne .SkipRight
        cpx #130
        bcs .SkipRight
        clc
        lda #20
        adc XPosP0
        tax 
        inc InputCount
.SkipRight
	stx XPosP0
.ButAlPressed
	rts
        
;;;Sprites and colors;;;
	align $100
        ;Field, stable height 92
FieldData
        hex 000000 000000 000000 ffffc0 211040 211040 ffffc0
        hex 211040 211040 ffffc0 211040 211040
        hex ffffc0 211040 211040 ffffc0 211040 
        hex 211040 ffffc0 000000 000000
        hex 000000 000000 000000 
NumData
        .byte $EE,$AA,$AA,$AA,$EE	;0
        .byte $22,$22,$22,$22,$22	;1
        .byte $EE,$22,$EE,$88,$EE	;2
        .byte $EE,$22,$66,$22,$EE	;3
        .byte $AA,$AA,$EE,$22,$22	;4
        .byte $EE,$88,$EE,$22,$EE	;5
        .byte $EE,$88,$EE,$AA,$EE	;6
        .byte $EE,$22,$22,$22,$22	;7
        .byte $EE,$AA,$EE,$AA,$EE	;8
        .byte $EE,$AA,$EE,$22,$EE	;9
	.byte $EE,$AA,$EE,$AA,$AA	;A
        .byte $CC,$AA,$CC,$AA,$CC	;B
        .byte $EE,$88,$88,$88,$EE	;C
        .byte $CC,$AA,$AA,$AA,$CC	;D
        .byte $EE,$88,$EE,$88,$EE	;E
        .byte $EE,$88,$EE,$88,$88	;F
Frame0P0
        .byte #0
        .byte #%00100100
        .byte #%00111100
        .byte #%11111111
        .byte #%00100100
        .byte #%00011000 
Frame0P1
	.byte #0
        .byte #%11111111
       	.byte #%11000011
        .byte #%11000011
        .byte #%11000011
        .byte #%11000011
        .byte #%11000011
        .byte #%11000011
        .byte #%11111111
Frame1P1
	.byte #0
        .byte #%11111111
       	.byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111
        .byte #%11111111

;end
	org $fffc
        .word Start	
        .word Start	
