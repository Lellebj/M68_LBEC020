startsp:        equ $F8000
Rbuff		    equ	$F8040
startusp:		equ	$F7300

	********* Interrupt Mask Values
disableints:    equ 	$2700		;	interrupt mask = 7
enableints:     equ 	$2000		; interrupt mask = 0

vbuserr: 		equ $08		;user-gen bus error
vadderr:		equ	$0C		;user-gen address error
villeginst:		equ	$10		;user-gen illegal instruction
vzerodiv:		equ	$14		;following three exceptions mapped to a generic exception service routine    
vchk:			equ	$18
vtrapv:			equ	$1C
vprivinst:		equ $20		;user-gen privilege violation
vtrace:			equ $24		;single stepping
valinevec		equ $28
vflinevec		equ $2C
vformaterr		equ $38
vuninitvec		equ $3C
vspuriousvec	equ $60
xhandlebase		equ	$3C 	;complete table with generic exception survive routine
endvectors:		equ $3FC

;*****************************************************************
; Autovectors & interrupt vectors
;-------------------------------------------------------------

vautoV1:		equ 25*4	;$64
vautoV2:		equ 26*4	;$68
vautoV3:		equ 27*4	;$6C
vautoV4:		equ 28*4	;$70
vautoV5:		equ 29*4	;$74
vautoV6:		equ 30*4	;$78
vautoV7:		equ 31*4	;$7C

;*****************************************************************
; Trap vectors
;-------------------------------------------------------------
vtrap00			equ 4*32		;$80
vtrap01			equ 4*(32+1)	;$84
vtrap02			equ 4*(32+2)	;$88
vtrap03			equ 4*(32+3)    ;$8C
vtrap04			equ 4*(32+4)    ;$90
vtrap05			equ 4*(32+5)    ;$94
vtrap06			equ 4*(32+6)    ;$98
vtrap07			equ 4*(32+7)    ;$9C
vtrap08			equ 4*(32+8)    ;$A0
vtrap09			equ 4*(32+9)    ;$A4
vtrap10			equ 4*(32+10)   ;$A8
vtrap11			equ 4*(32+11)   ;$AC
vtrap12			equ 4*(32+12)   ;$B0
vtrap13			equ 4*(32+13)   ;$B4
vtrap14			equ 4*(32+14)   ;$B8
vtrap15			equ 4*(32+15)	;$BC

;*****************************************************************
; User interrupts
;-------------------------------------------------------------

vtimer:			equ 64*4	;#256 = $100
vtimer_4:       equ 80*4    ;#320 = $140        ; vectored interrupt #80 ($140) on IRQ line 4
vduart:         equ 90*4    ;#360 = $168        ; vectored interrup #90 ($168) on IRQ line 3
vZ8536T1:       equ 100*4   ;#400 = $190        ; vectored interrup #100 ($190) on IRQ line 3
vZ8536T2:       equ 98*4    ;#392 = $188        ; vectored interrup #98 ($188) on IRQ line 3
vZ8536T3:       equ 96*4    ;#384 = $180        ; vectored interrup #96 ($180) on IRQ line 3
vZ8536PA:       equ 110*4    ;#440 = $1BB        ; vectored interrup #110 ($1BB) on IRQ line 3
vZ8536PB:       equ 112*4    ;#448 = $1C0        ; vectored interrup #112 ($1C0) on IRQ line 3

D_50ms  = $61A8
D_20ms  = $2710
D_15ms  = $1D4C
D_10ms  = $1388
D_5ms   = $9C4
D_1_5ms = $2EE
D_1ms   = $1F4
D_500us = $FA
D_100us = $32
D_50us	= $20

