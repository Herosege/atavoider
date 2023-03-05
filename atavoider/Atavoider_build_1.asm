	processor 6502
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"

;variables
Counter		equ $81
SpriteHeight	equ 5

        seg.u Variables
	org $80
XPos		.byte
YPos		.byte
InputCount	.byte
Temp		.byte
FieldIdx	.byte
FieldCount	.byte

SpritePtr	.word
FieldPtr	.word

TmpPlrBitMap	.byte
TmpPlayerY	.byte

;code
	seg Code
        org $f000
Start
	CLEAN_START
        ;Pointers
        lda #<Frame0
        sta SpritePtr
        lda #>Frame0
        sta SpritePtr+1
        lda #<FieldData
        sta FieldPtr
        lda #>FieldData
        sta FieldPtr+1
        
	lda #195
        sta YPos
        lda #76
        sta XPos

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
        lda #0
        sta FieldIdx
        lda YPos
        sta TmpPlayerY
               
        ;Position X 
        lda XPos ;3-161 
        ldx #0
        jsr SetPositionX
        sta WSYNC
        sta HMOVE
        
        TIMER_WAIT
        lda #0
        sta VBLANK
       
	TIMER_SETUP 192
  
FieldSeg
	ldy FieldIdx
	lda (FieldPtr),y
        beq SegEnd
        sta FieldCount
        iny
        lda (FieldPtr),y
        tax
        iny
        lda (FieldPtr),y
        sta Temp
	iny
        lda (FieldPtr),y
        iny
        sty FieldIdx
        tay
        sta WSYNC
        stx PF0
        lda Temp
        sta PF1
        lda TmpPlrBitMap
        sta GRP0
        sty PF2
        ldx FieldCount
SpriteLoop
	
	lda #SpriteHeight
        inc TmpPlayerY
        sbc TmpPlayerY
        bcs DrawSprite
        lda #0
DrawSprite 
        asl
        tay
        lda (SpritePtr),y
        sta TmpPlrBitMap
        iny
        lda (SpritePtr),y
	sta WSYNC
        sta GRP0
        lda #$2f
        sta COLUP0
        dex
        beq FieldSeg
        sta WSYNC
        lda TmpPlrBitMap
        sta GRP0
        jmp SpriteLoop
SegEnd
	lda #$0
        sta COLUBK
        TIMER_WAIT
        
        TIMER_SETUP 29
        lda #2
        sta VBLANK
        jsr GetInput
        TIMER_WAIT
	
        ;End frame
        
        jmp NextFrame
        
;;;Subroutines, aka functions;;;

;XPos
SetPositionX
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
	ldx YPos
	lda #$10
        bit SWCHA
        bne .SkipUp
        cpx #225
        bcs .SkipUp
       	clc
        lda #13
        adc YPos
        tax
        inc InputCount
.SkipUp
        lda #$20
        bit SWCHA
        bne .SkipDown
        cpx #170
        bcc .SkipDown
        clc
        lda #-13
        adc YPos
        tax 
        inc InputCount
.SkipDown
	stx YPos
        ;Left - Right
        ldx XPos
        lda #$40
        bit SWCHA
        bne .SkipLeft
        cpx #25
        bcc .SkipLeft
        clc
        lda #-20
        adc XPos
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
        adc XPos
        tax 
        inc InputCount
.SkipRight
	stx XPos
.ButAlPressed
	rts
        
;;;Sprites and colors;;;
	align $100
        ;Field, stable height 92
FieldData
        .byte 15,	#%00000000,#%00000000,#%00000000
        .byte 3,	#%11000000,#%11111111,#%11111111
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 2,	#%01000000,#%00010000,#%00100001
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 3,	#%11000000,#%11111111,#%11111111
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 2,	#%01000000,#%00010000,#%00100001
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 3,	#%11000000,#%11111111,#%11111111
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 2,	#%01000000,#%00010000,#%00100001
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 3,	#%11000000,#%11111111,#%11111111
       	.byte 4,	#%01000000,#%00010000,#%00100001
        .byte 2,	#%01000000,#%00010000,#%00100001
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 3,	#%11000000,#%11111111,#%11111111
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 2,	#%01000000,#%00010000,#%00100001
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 3,	#%11000000,#%11111111,#%11111111
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 2,	#%01000000,#%00010000,#%00100001
        .byte 4,	#%01000000,#%00010000,#%00100001
        .byte 3,	#%11000000,#%11111111,#%11111111
        .byte 1,	#%00000000,#%00000000,#%00000000
        .byte 0
	
Frame0
	.byte #0
        .byte #0
        .byte #%00100100
        .byte #%00100100
        .byte #%00111100
        .byte #%00111100
        .byte #%11111111
        .byte #%00100100
        .byte #%00100100
        .byte #%00011000
;end
	org $fffc
        .word Start	
        .word Start	