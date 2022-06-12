

	include include/68230_68681_ADR.inc
	include include/m68k_MACROS.inc
	include include/exception_vector_addresses.inc
	include include/colors_IL9320.inc

	section .assemblycode




Main::	
;*********************************************		
		lea 	Base68681,A4

		move.b	#$FF,SOPR(A4)	; reset all
		move.b	#%00010001,ROPR(A4)	; turn on red/green

		;------- Testprogram addess error
		move.l		sp,$F800C
		; move.l		#$201234,($F8020)				; ref
		move.l		#$11122233,D1					;
		move.l		D1,$F8010					;
		move.l		$201234,D1					; bus error - program ref

		trap #2
		; trapv
		move.w		#$5000,D0
		addi.w		#$4000,D0			; V will be set
		; trapv
		; divs		#0,D0				; divide by zero

		; move.l		$200000,D5
		; dc.w		$4AFC
		; andi		#34,SR

		; dc.w		$F91F
		; CHK
		move.w		#$03,D1
		move.w		#$40,D2
		move.w		#$031,D0
		; D0 rep upper bound
		chk			D0,D1
		chk			D0,D2				; chk exception

		trap #0


;*********************************************		
		lea		Base68230,A2

			move.w 		#T_NOINT|T_TIN_P|T_Z_PRELOAD,-(sp)
						; No interrupt/ Preload/ Pin32count/Prescaler
			
			move.w 	#10,-(sp)	 
						;( 0,5 sec @ E=1.0 MHz & prescaler 32)
			move.w	#80,-(sp)
						; vector #80 - vector adr $140
			jsr		initTimer68230


		lea 		$604560,A0
		move.b	#$88,(PBDR,A2)
		move.b	#$02,(PADR,A2)			; clear pulse
		move.b	#$00,(PADR,A2) 
		move.b	#$02,(PADR,A2) 
		move.b	#$03,(PADR,A2)			; clear low - start count
	
		;------- Testprogram **************************
		


		;------- Testprogram addess error
		move.l		sp,$F800C
		; move.l		#$201234,($F8020)				; ref
		move.l		#$11122233,D1					;
		move.l		D1,$F8010					;
		move.l		D1,$F8014					;
		; move.l		$201234,D1					; bus error - program ref
		

		move.l		D1,($F8018)					;
		move.l		D1,($F801C)					;
		move.l		D1,($F8020)					;
		move.l		D1,($F8024)					;
		; jmp			(A0)					;bus error - program ref

ledtest4:
		move.b	#$81,(PBDR,A2)			; 
		move.b	#$02,(PADR,A2)			; stop count
		move.b	#$02,(PADR,A2)			; stop count
		; bra 	ledtest4

		; stop #$2700

;*********************************************		

			move.w 	#T_INT_V|T_TIN_P|T_Z_PRELOAD,-(sp)	
						; Vector int(#4/ $70)/ Preload/ Pin32count/Prescaler  pin32 with prescaler
			; move.w 		#T_NOINT|T_TIN_P|T_Z_PRELOAD,-(sp)
						; No interrupt/ Preload/ Pin32count/Prescaler
			
			move.w 	#08,-(sp)	 
						;( 0,5 sec @ E=1.0 MHz & prescaler 32)
			move.w	#80,-(sp)
						; vector #80 - vector adr $140
			jsr		initTimer68230
			jsr		startTimer68230


		move.b 	#0,PBDR(A2)
		clr.l	D2
tlp0:
		move.b	D2,(PBDR,A2)

		addq	#1,D2

		move.l	#$2FFFFF,D3
		move.l	#$2FF00FFF,D3
		move.l	#$2EFFF,D3
		clr.l	D3				; resets the N flag

tlp1:
		
		; subq.l	#1,D3
		; bne.s	tlp1 
		
		; jsr 	testTimeout			; check if 68230 timer has timeout ? => Z  set!
		; beq.s	tlp1				; - Timeout NOT reached ;set Z bit
		nop
		nop
		bmi		20$
		bra 	tlp1
	

		stop #$2000
		nop
		nop
		move.l	#00,($22+startsp)
		

20$:	jsr 	resetTimer68230

		bra		tlp0

		bra 	ledtest1

		align 3

		; trapv
		move.w		#$5000,D0
		addi.w		#$4000,D0			; V will be set
		; trapv
		; divs		#0,D0				; divide by zero

		move.l		$200000,D5
		; dc.w		$4AFC
		; andi		#34,SR

		; dc.w		$F91F
		; CHK
		move.w		#$03,D1
		move.w		#$40,D2
		move.w		#$031,D0
		; D0 rep upper bound
		chk			D0,D1
		chk			D0,D2				; chk exception
		
ledtest1:
		bra 	ledtest1


		dc.l	$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0

		; move.l		(A0),a1
		;++++++++++++++++++++++++++++++++++++
*****************************************************************


		even
	section C_8536Init

	include include/CIO_Main_Control_Registers.inc
		    ;TEST,CIO
        	;OBJ CODE M STMT SOURCE STATEMENT
        	;CIO TEST PROGRAM

BAUD:   equ     9600    		;ASYNC BAUD RATE
RATE:   equ     BAUD/100
CIOCNT: equ     576/RATE		; ...6
CIO_Init_data:	equ $F100




Z8536_Init:
CIOINI:
		; movem.l	A2-A4/D1-D4,-(sp)
**************** init code of Z8536 chip  ********
		lea		Base8536,A4
		move.l	sp,$F8010

		lea		CIO_Init_data,A3
		lea		Base8536,A4
		move.b	(CIOCTL,A4),D2			; INSURE STATE 0

		move.b	#00,(CIOCTL,A4)			;WRITE PTR OR CLEAR RESET
		move.b	(CIOCTL,A4),D2			; STATE 0
		move.b	#00,(CIOCTL,A4)			; WRITE PTR
		move.b	#01,(CIOCTL,A4)			; WRITE RESET
		move.b	#00,(CIOCTL,A4)			; CLEAR RESET


**************** send the list of init code  ********
		lea		CLST,A2		
		clr.l	D1
		move.l	#CEND-CLST-1,D1
		clr.l	D2
.SList:		
		move.b 	(A2)+,D2
		move.b	D2,CIOCTL(A4)
		DBRA	D1,.SList			; send the list of init code

**************** send the list of init code  ********

		; move.b	#_CT1CS,CIOCTL(A4)	; CT Commande and status
		; move.b	CIOCTL(A4),$F8030	
		; move.b	#_CT1CS,CIOCTL(A4)	; CT Commande and status
		; move.b	#%110<<5|_GCB,CIOCTL(A4)

		; move.b	#_CTMS1,CIOCTL(A4)	; CT Mode specification
		; move.b	CIOCTL(A4),$F8032	

		; move.b	#_CTTC1L,CIOCTL(A4)	; CT Time constant reg.
		; move.b	CIOCTL(A4),$F8034	
		; move.b	#_CTCC1L,CIOCTL(A4)	; CT Current Count reg.
		; move.b	CIOCTL(A4),$F8035	
		; move.b	#_CTTC1M,CIOCTL(A4)	; CT Time constant reg.
		; move.b	CIOCTL(A4),$F8036	
		; move.b	#_CTCC1M,CIOCTL(A4)	; CT Current Count reg.
		; move.b	CIOCTL(A4),$F8037	


		; move.l	#$5A,D1
 		; move.b	D1,CIOA(A4)

		; move.b  	#_CVR,CIOCTL(A4)
		; move.b		CIOCTL(A4),$F8040
		; move.b  	#_IVRCT,CIOCTL(A4)
		; move.b		CIOCTL(A4),$F804C
		
		trap #2
otst:

		move.b	#_CT1CS,CIOCTL(A4)
		move.b	_GCB,CIOCTL(A4)

 		move.b	D1,CIOA(A4)
		
		move.l	#$1FFFF,D4
.dellop:
		move.l	#$FFFFFF,D3
		; subq.l	#1,D4
		; bne		.dellop


		move.b	#_CT1CS,CIOCTL(A4)
		move.b	CIOCTL(A4),D1
 		move.b	D1,CIOA(A4)


		nop
		nop

		trap #0


		bmi		22$
		move.w	#$CC,$F8028			; indicate inside loop


		bra 	.dellop

22$:
		move.w	#$11,$F8028		; indicate outside loop
		DBRA	D1,otst

		; move.w	$5001,D3

		move.b	#_CT1CS,CIOCTL(A4)
 		move.b	#0,CIOCTL(A4)

		; stop #disableints

		movem.l	(sp)+,A2-A4/D1-D4


		trap 	#0


		align 3
CLST:			; start of init data		
		dc.b	_PMSA					; PORT A MODE	bitport
		dc.b	$00
		dc.b	_PMSB					; PORT B MODE	bitport
		dc.b	$00
		dc.b	_DDA					; PORT A DIRECTION 0=output
		dc.b	%00000000
		dc.b	_DDB					; PORT B DIRECTION 0=output
		dc.b	%11101110
		dc.b	_DDC					; PORT C DIRECTION
		dc.b	%00001110
		
		dc.b	_MIC
		dc.b	_MIE					; master interrupt ON

		dc.b	_IVRCT
		dc.b	96						; Interrupt vector # 96/98/100 for CT3/2/1

		dc.b	_CTMS1					; CT1 MODE;Counter/Timer 1’s Mode Specification
		dc.b	_C_SC|_EOE|_ECE|_PULSE	; CT1 out pin (B)12

		dc.b	_CTMS2					; CT2 MODE;Counter/Timer 2’s Mode Specification
		dc.b	_C_SC|_EOE|_ECE|_PULSE	; CT2 out pin (B)8

		dc.b	_CTMS3					; CT3 MODE;Counter/Timer 3’s Mode Specification
		dc.b	_C_SC|_EOE|_SQUAREWAVE		; CT3 single loop, no output (C)19

		dc.b	_CTTC1M					; CT1 TC MSB;Counter/Timer l’s Time Constant-MSBs
		dc.b	$10
		dc.b	_CTTC1L					; LSB
		dc.b	$44; CIOCNT
		dc.b	_CTTC2M					; CT2 TC MS8Counter/Timer 2’s Time Constant-MSBs
		dc.b	$02
		dc.b	_CTTC2L					; LSB
		dc.b	$33;CIOCNT
		dc.b	_CTTC3M					; CT3 TC MSBCounter/Timer 3’s Time Constant-MSBs
		dc.b	$10
		dc.b	_CTTC3L					; LSB
		dc.b	$AA					;CIOCNT

										;COUNTER/TIMER 1,2,3 ENABLE

		dc.b	_MCC					; Master Config. REG.
		dc.b	_PAE|_PBE|_CT1E|_CT2E|_CT3E	;PORT A&B&CT1 ENABLE 
		; dc.b	_PAE|_PBE|_CT1E				;PORT A&B ENABLE 

		dc.b	_CT1CS					;* CT1 TRIGGERCounter/Timer 1’s Command and Status
		dc.b	_SET_IE|_GCB|_TCB		;GATE COMMAND BIT (GCB)|TRIGGER COMMAND BIT (TCB) (WRITE ONLY - READ RETURNS 0)
		dc.b	_CT2CS					; CT2 TRIGGER  same as 1
		dc.b	_GCB|_TCB
		dc.b	_CT3CS					; CT3 TRIGGER  same as 1
		dc.b	_GCB|_TCB

CEND: 	


run
*****************************************************************
*****************************************************************





		even
		section C_Kermit_test
Kermit_start::

		move.l	A0,-(sp)
K1$:
		lea 	kerm_msg1,a0		
		jsr		putstring_cr
		lea 	star_row_msg,A0
		jsr		putstring
		jsr		putstring_cr

		lea		InText,A0		; read from terminal; result in InText
		jsr		getstring

		pea 	InText
		move.l	#$3100,-(sp)
		jsr		strcopy

		pea		InText
		pea		kerm_str
		jsr 	strcompare		;correct input string  cmp. 'pwstring' and 'Cstr2'

		cmpi.b	#0,D0
		beq.s	K2$


		lea		test_msg,A0		; input and try again
		jsr		putstring		; message withour CR/LF!	
		bra		K1$

K2$:
		lea		rec_str,A0
		jsr		putstring
		jsr		newline

		lea		InText,A0		; read from terminal; result in InText
		jsr		getstring

		pea 	InText
		move.l	#$3100,-(sp)
		jsr		strcopy


		move.l	(sp)+,A0
		rts

		even
kerm_msg1:
		dc.b	'Trying reading from kermit:   ',0
		even
kerm_str:
		dc.b	'kermit -ir',0,0,0		
		even
rec_str:
		dc.b	$01,$23,$20,$4E,$33,0,0,0
		even


*****************************************************************

; Checksum calculation

; The following example record:

; S1137AF00A0A0D0000000000000000000000000061

; is decoded to show how the checksum value is calculated as follows:

;     Add: Add each byte 0x13 + 0x7A + 0xF0 + 0x0A + 0x0A + 0x0D + 0x00 + ... + 0x00 = 0x19E total.
;     Mask: Keep the least significant byte of the total = 0x9E.
;     Complement: Compute ones' complement of least significant byte = 0x61.

; 16-bit memory address

; S00F000068656C6C6F202020202000003C
; S11F00007C0802A6900100049421FFF07C6C1B787C8C23783C6000003863000026
; S11F001C4BFFFFE5398000007D83637880010014382100107C0803A64E800020E9
; S111003848656C6C6F20776F726C642E0A0042
; S5030003F9
; S9030000FC
		even
	section SST39WR

		movem.l	A2-A4,-(sp)

		movea.L	#$AF000A,A2
		clr.L	D1
		move.w	#$7C6E,D1
		jsr 	push_to_SST39

		movea.L	#$AF0004,A2
		move.w	#$1234,D1
		jsr 	push_to_SST39

		movem.l	(sp)+,A2-A4

		trap #0


push_to_SST39:
		; // A2.l contains adress, D1.w contains data


		move.L	#$AFAAAA,A3
		move.L	#$AF5554,A4

		move.w	#$AAAA,(A3)
		move.w	#$5555,(A4)
		move.w	#$A0A0,(A3)
		move.w	D1,(A2)

		move.w	(A2),D2
		andi.w	#$C0C0,D2

flash_loop:
		move.w	D2,D1
		move.w	(A2),D2
		andi.w	#$C0C0,D2
		eor.w	D2,D1			; will be non zero if still toggling....
		bne.s 	flash_loop

		rts


	
	rorg $200				; erase section - starts at $4100

erase_sect:
		movem.l	A2-A4,-(sp)
		movea.l	#$AF0004,A2
		
		move.l	#$AFAAAA,A3
		move.l	#$AF5554,A4

		move.w	#$AAAA,(A3)
		move.w	#$5555,(A4)
		move.w	#$8080,(A3)
		move.w	#$AAAA,(A3)
		move.w	#$5555,(A4)
		move.w	#$3030,(A2)


sector_erase_flash_loop:

		move.w	(A2),D2
		cmp.w	#$FFFF,D2
		bne.s	sector_erase_flash_loop

		movem.l	(sp)+,A2-A4

		trap #0


		even
	section LAB_Program
		movem.l	A2-A4,-(sp)
		; BSET	#0,TCR(A2)			; start timer 68230
		; bset	#0,WCR2(A5)		  	**** address CR#1
		; bset	#0,WCR1_3(A5)		**** Reset counter
		; bclr	#0,WCR1_3(A5)		**** Reset counter
		; BSET	#6,WCR2(A5)			; start timer #2 6840
		jsr 	startTimer68681
		jsr		startTimer68230
		lea		Base68230,A2		; A2 point to 68230

		; trap #2

		jsr		clearIL9320
		lea		Base68681,A4

		move.w	#$02,-(sp)		; coumn - xpos
		move.w	#$01,-(sp)		; row  - ypos
		pea		run_msg
		jsr		stroutIL9320

		jsr		newline
		lea		run_msg,A0
		jsr		putstring_cr
		lea		run_msg,A0
		jsr		putstring
		lea		e4ttt,A0
		jsr		putstring
		jsr		newline


etl:
		bra.s 	nlt2

e4ttt:	dc.b	'ABCDEFGHIJK',$1b,$5B,$44,$1b,$5B,$44,$1b,$5B,$44,0,0,0
		even




		; dc.l $11223344,$00550066,$778899AA,$BBCCDDEE
		; dc.l $11223344,$00550066,$778899AA,$BBCCDDEE
		; dc.l $11223344,$00550066,$778899AA,$BBCCDDEE
		; dc.l $11223344,$00550066,$778899AA,$BBCCDDEE
		; dc.l $11223344,$00550066,$778899AA,$BBCCDDEE

nlt2:
		move.l	#$20,D5
		; trap #2


stploop:	
		move.b 	D5,PADR(A2)
		move.b	STAC(A4),D0			; start counter
		; move.b	#%1000,IMR(A4)		; start timer 68681

		; trap #2
		move.l	D5,D0
		pea		Cstr1
		jsr		bintodecstr		; value of D5 in dec.str.Cstr1

		move.l	#$A0002,-(sp)		; col,rot - 10,2
		pea		Cstr1
		jsr		stroutIL9320
		clr.l	D0				; resets the N flag

.T_poll:

		nop
		nop
		bmi		20$
		bra 	.T_poll

		;move.b (CRM,A2),D1
		;move.b	D1,(PADR,A2)

20$:	; BSET 	#0,TSR(A2)			; reset status (again)
		
		DBRA 	D5,stploop
		

		BCLR	#6,WCR2(A5)			; stop timer #2 6840

		move.l 	#60,D5
		
		jsr		stopTimer68230
		jsr		stopTimer68681

		; BCLR	#0,TCR(A2)			; stop the 68230 timer
		; move.b	STOC(A4),D0			; stop counter 68681

		; trap 	#2					; show regs		
		movem.l	(sp)+,A2-A4


		trap 	#0					; Return to 'monitor'





		**************  '1 minute finish!',0 ************
		move.w	#$03,-(sp)		; row(=02) and column (=03)
		move.w	#$02,-(sp)		; row(=02) and column (=03)
		pea		test_msg3		;'1 minute finish!',0
		jsr		stroutIL9320

		move.w	#2,-(sp)
		jsr		wait_n_secs
		lea 	IO_LS373,A3
		move.b 	#$C0,(1,A3)
		

		; clear screen with red background
		move.w	#RED,-(sp)
		jsr		setbgcIL9320
		move.w	#WHITE,-(sp)
		jsr		setfgcIL9320
	
		jsr		clearIL9320

		move.L #$40,D5
9$:
		move.l D5,D0
		trap #1

		lea 	IO_LS373,A3
		move.b 	D5,(1,A3)
10$:		
		addi.w	#$40,D5
		bra		10$
		cmpi.w	#$255,D5

		ble		9$


temptest::
		; clear screen with red background

		move.l	#$55,D0
		trap #0
		move.w	#3,-(sp)
		jsr		wait_n_secs

		; jsr 	clearLCD
		move.l	#$AAAA,D0
		trap #0
		move.w	#2,-(sp)
		jsr		wait_n_secs

		; jsr 	clearLCD
		move.l	#$AEAEAE,D0
		trap #2
		move.w	#3,-(sp)
		jsr		wait_n_secs


		; jsr 	clearLCD
		move.l	#$12345689,D0
		trap #0
		move.w	#2,-(sp)
		jsr		wait_n_secs


		stop #$2700
	
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		; dc.L	$11113333,$A5A5A5A5,$0F0F0F0F,$96969696
		


				;++++++++++++++++++++++++++++++++++++
*****************************************************************
