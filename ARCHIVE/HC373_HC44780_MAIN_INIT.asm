;
;	Init_HC44780_start::	
;	clearLCD::
;	setcursor::
;	write_command:
;	stroutlcd:
;	write_data:		; D0-value; D1-mode 1(data), 0(command)
;	send:                 ; D0-value; D1-mode 1(data), 0(command)
;	write4bits:   
;	wait_n_secs:
;	Wait_delay_time:


	include include/HC373_HD44780_defs.inc
	include include/68230_68681_ADR.inc
	include include/exception_vector_addresses.inc
	include include/colors_IL9320.inc

ASEMBL_68681 equ 1			; activate =1 if circuit 68681 is used	 	
ASEMBL_6840 equ 0			; activate =1 if circuit 6840 is used
ASEMBL_LEDSTART  equ 0		; activate =1 if no uART is used, only led indications	 	
ASEMBL_IL9320	equ 0			; activate =1 if display 9320 is used

****************************************************************
		section  initvectors
****************************************************************
***   Prepare the start vector and stack pointer    		
****************************************************************

		dc.l startsp
		dc.l	start
		; dc.l	Rscndtest
		; dc.l _MonitorProgram


*************************************************
		; section .monitor
****************************************************************
***   Start/Init of prog sequence    		
****************************************************************

		xref 	setcursor,Main_68230_init,Init_68010_vectors

start::

		move.l	#$1A2B3C4E,D1
		move.l	D1,$100200
		move.l	#$98765432,D2
		move.l	D2,$800100
		move.l	$800100,D3
		move.l	D3,$100210

tl1:	bra  tl2
tl2:
		move.l	#0,A0
fillzero:
		move.l	#$1,(A0)+
		cmpa.l	#$3FF,A0
		bls.s	fillzero

	*** ;	prep_stacks
	*** ; zero out supervisor and user stack	
		move.l	#startsp+$100,A1		; ¤F7D00 - F8100
		move.l	#startusp+$100,A2		; ¤F7000 - F7400

		move.l	#00,D1
		move.l	#$400,D2		; define 4k ram area
.10:		
		clr.l	-(A1)		; Supervisor stack
		clr.l	-(A2)		; User stack
		DBRA	D2,.10


		lea 	startsp,sp
		move.w	#$2700,SR			; int level  level 0
		jsr 	prep_HEAP_SPACE			; prepare the HEAP strings in RAM.
		jsr		prep_key_term_buf		; prep buffers for key and terminal

		jsr 	Init_68010_vectors			; setup the vector table
.13:	lea 	IO_LS373,A3
		move.b 	#$02,(1,A3)

		lea		Base68230,A2
		; bra .13

*************************************************

;-----------------------------------------------
;	68230 INIT
;***********************************************
		jsr		Main_68230_init
;	wait 2 sec
		;move.w	#1,-(sp)
		;jsr		wait_n_secs
		;jsr 	clearLCD
		;jmp 	temptest

		jsr		Main_68230_Timer_init
		; jsr		SetThe68230Timer
		move.b 	#$03,(1,A3)

		if ASEMBL_68681=1
			lea		Base68681,A4

			jsr 	Main_UART_68681_init
			move.b 	#$04,(1,A3)

			move.b	#$FF,SOPR(A4)	; set all compl 0xFF = 00
			move.b	#$01,ROPR(A4)	; reset 
			move.b 	#8,PADR(A2)

		endif
*************************************************
		*** Show init progress if ASEMBL_IL9320=1
		********************
		if ASEMBL_IL9320=1
			move.w	#RED,-(sp)
			jsr		setbgcIL9320
			jsr		clearIL9320


			move.l	#$060002,-(sp)		; row, coumn - xpos
			pea		INIstring
			jsr		stroutIL9320
		endif
*************************************************
		if ASEMBL_LEDSTART=1
		JMP  Main			; if indication by LED ONLY jump to start $1000
		endif
*************************************************


		move.l	#$40,D5

		; move.b	STOC(A4),D0			; stop counter
		; move.b	#1<<3,IMR(A4)
		move.b	#0,IMR(A4)
		move.b	#03,rec_trInt.l
		move.b	#02,recOnlyInt.l
		clr.w	allIntOff.l
		clr.w	collect_esc_seq

		clr.w	counterActive		; indicate 681 counter interrupt off

	*** keyboard buffer setup
		move.l	#keyInputBuffer,keyBufPtr ; point to the buffer for key input
		move.l	#termOutputBuffer,termBufPtr ; point to the buffer for terminal output

		; BSET	#0,TCR(A2)			; start timer
		BCLR	#0,TCR(A2)			; stop timer
		;****************	Clear LCD and set BG color

		jsr 	initPort				; setup serial communiq teminal+minicom
		move.b 	#7,PADR(A2)


; 	;******************************************
; 	; Log in to system..
; 	;******************************************
31$:
		lea 	test_msg4,a0		;dc.b	'Welcome to the system (LB68K)    ',0
		jsr		putstring_cr
		lea 	star_row_msg,A0
		jsr		putstring
		jsr		putstring_cr
		bra.s	21$

		lea		InText,A0		; read from terminal; result in InText
		jsr		getstring

		pea		InText
		pea		pwstring
		jsr 	strcompare		;correct input string  cmp. 'pwstring' and 'Cstr2'

		cmpi.b	#0,D0
		beq.s	21$

		lea		test_msg,A0		; input and try again
		jsr		putstring		; message withour CR/LF!	
		bra 	31$

*********  Welcome msg ****************
21$:

		lea		SYS_msg1,A0
		jsr		putstring_cr
		lea		SYS_msg2,A0
		jsr		putstring_cr
		lea		SYS_msg3,A0
		jsr		putstring_cr

		****  Change MSG LCD .
		move.w	#CYAN,-(sp)
		jsr		setbgcIL9320
		jsr		clearIL9320
		move.l	#$060002,-(sp)		; row, col (6,2)  - ypos
		pea		run_msg
		jsr		stroutIL9320


	**********************************************************
		****  Change MSG LCD .
		move.w	#CYAN,-(sp)
		jsr		setbgcIL9320
		jsr		clearIL9320
		; move.l	#$060002,-(sp)		; row, col (6,2)  - ypos
		; pea		InText
		; jsr		stroutIL9320
	*********************************************************
		jmp		Warmstart

*************************************************
		section .monitor
****************************************************************




		xdef	Warmstart
Warmstart:
		jsr		newline

		lea		Base68230,A2
		lea 	IO_LS373,A3
		lea		Base68681,A4

		move.w	#$70,IO_LS373		; LCD shows main monitor prog!
		move.b 	#$11,PADR(A2)

************************************************************************
		; jmp Main		; go to Main prog   sect .assemblycode
************************************************************************
**----------------------------------------------------------------------
		jsr		SetThe68230Timer
		jsr		SetThe68681Timer

		move.l	#Main,PCval		; the monitor prog PC value
		move.l	#$FC300,dumpStack_LO
		move.l	#$FC360,dumpStack_HI
		; move.l	#ComTex,dumpStack_LO
		; move.l	#$1200,dumpStack_HI

		move.b 	#$33,PADR(A2)

		; BSET	#0,TCR(A2)			; start timer
		BCLR	#0,TCR(A2)			; stop timer

41$:	;move.b	(RSR,A3),D1
		; move.b	(RT2C,A5),D2
		; move.b	D2,PADR(A2)


testprog1:
		move.w	#WHITE,-(sp)
		jsr		setbgcIL9320
		move.w	#BLACK,-(sp)
		jsr		setfgcIL9320
		
		jsr		clearIL9320

*************************************************
***	print PC address, its value and cursor
*************************************************
Cursor_Loop:
		jsr		newline
		move.b	#'P',D0
		jsr		putchar
		move.b	#'C',D0
		jsr		putchar
		move.b	#'[',D0
		jsr		putchar
		move.l	PCval,D0
		move.w	#6,D7			; 6 chars
		jsr		printhex		; prints 6 char address

		move.b	#']',D0
		jsr		putchar
		move.b	#'=',D0
		jsr		putchar
		lea		PCval,A0
		move.l	(A0),A0			; value of monitor prog counter		
		move.w	(A0),D0			; content in (PCval)
		clr.w	D1				; no char count
		
		jsr		hexWordOut

		lea		cmdprompt,a0		; request command for host system
		jsr		putstring

		lea		InText,a0		; read command from console
		jsr		getstring


		lea		InText,A0
		; pea		InText
		; pea		keyInputBuffer		; where to put the address table
		; jsr		strSplitIntext		; 'InText' 0-separated, index addresses from
									; 'InText+ $keyInputBuffer
		jsr		newline

	***************************************************
	***	Loop through the commands in string, abort if error
	*** compared with keywords from (ComTex)
	************************************************************

exeComm:
	;*** A0		; A0 point to InText

		jsr		skipPriorBlanks
		move.l	A0,A3				; save start of act. string for later		
		tst.b	(A0)					; only blanks in string
		beq 	Cursor_Loop

		lea		ComTex,A2			; first word in command dic (ComTex)
E_loop$:
		move.l	A3,A0
	***	A2	 points to keywords
	***	A0  point to commands in InText
		
E3$:	
		tst.b	(A2)				; end of keyword encounterd ?
		beq.s	.match			; keyword match!	
		** Mask to upper case if 'a' < x < 'z'
		cmpi.b	#'a',(A0)
		blt.s	E4$				;< 0x61
		cmpi.b	#'z',(A0)
		bgt.s	E4$				;> 0x7A
		bclr.b 	#5,(A0)			; turn to upper case...

E4$:	cmpm.b	(A0)+,(A2)+
		beq.s	E3$
	
	***	;No match with keyword , 	
	***	;step A2 to next keyword
E2$:	cmpi.b	#ECX,(A2)+
		bne.s	E2$	
		cmpi.w	#ECM,(A2)			; last keyword
		bne		E_loop$				; check more keywords

		bra		nomatch				; no keywords match, break loop

***********************************************************************
.match:			;Found keyword in string
***********************************************************************
	;***	Get offset in jumptable
	clr.w	D3
	move.b	1(A2),D3
	lsl.w	#2,D3			;mult by 4, result D3

	;***	A0 = first char in valuestring
	;***	read value following keyword in InText (stringbuffer)
		move.l	A0,A3				; save value address 	
		jsr		FindNextBlank
	;***	Outdata: A0->  fists char after ' ', or $FFFFFFFF (no blank)

		exg		A0,A3			; A0 & A3 switch; A3-> first after ' '
		jsr		readHexValfromString
	;*** Outdata: (A0)bit7 set if Non Hex char and NZ set, result in D0,D4
		bne.s	valueerror		; NZ if bit 7 is set

	;***	Jump to choosen command routine
	;************************************
		movem.l	A0/A1/D3,-(sp)

		lea		jumptable,A1
		move.l	(0,A1,D3.w),A0
		jsr		(A0)

		movem.l	(sp)+,A0/A1/D3

		move.l	A3,A0
		cmpa.l	#$FFFFFFFF,A3		; last command in InText
	
		bne		exeComm	
		bra		Cursor_Loop
	
***************************************************	
valueerror:
***************************************************	
		bclr.b	#7,(A0)

		move.b	#CHAR_CR,D0
		jsr		putchar
		jsr		putstring		; print (A0)
		move.b	#' ',D0
		jsr		putchar
		lea		msg_valuerr,A0
		jsr		putstring_cr
		jmp		Cursor_Loop


***************************************************	
FindNextBlank:		
***************************************************	
	;***	Indata: A0->	command in InText
	;***	Outdata: A0->  first char after ' ', or $FFFFFFFF (no blank)
		; movem.l	D0/A0-A4,-(sp)

	;***	Find next blank and mark as '0'
		
	;*** Find next blank	
		move.w	#' ',-(sp)
		move.l	A0,-(sp)			;pea		InText
		jsr		strchar
	;******		A0 points to ' '
		cmpa.l	#$FFFFFFFF,A0		; no blanks left
		beq		E5$

	;***	replace the ' ' with 0
		move.b	#0,(A0)
		addq	#1,A0				;skip past '0' A3-> rest of string
		; movem.l	(sp)+,D0/A0-A4
E5$:	
		rts

***************************************************	
readHexValfromString:   
***************************************************	
	;*** Indata: A0 -> value pos in string
	;*** Outdata: (A0)bit7 set if Non Hex char and NZ set
	;***	Outdata: D0/D4 = bin value
		move.l	A0,-(sp)			; store pos in string InTex
		move.l	A0,-(sp)			; A4-> start of value string
		jsr		hexstrtobin
		move.l	D0,D4
		move.l	(sp)+,A0
		btst.b	#7,(A0)			; indicate value error
		rts




***************************************************	
nomatch: ***	NO keyword match of actual part of string 
***************************************************
		move.b	#'"',D0
		jsr		putchar
		move.l	A3,A0
		jsr		putstring		; print unknown comm
		move.b	#'"',D0
		jsr		putchar
		lea		c_err_msg,A0
		jsr		putstring
;ESC [ Pn P,ESC [ 1 K
		jmp		Cursor_Loop


;****************************************************************
****	68230 Timer initializing
;****************************************************************

SetThe68230Timer:
		move.w 	#T_INT_V|T_TIN_P|T_Z_PRELOAD,-(sp)	
					; Vector int(#4/ $70)/ Preload/ Pin32count/Prescaler  pin32 with prescaler
		;move.b 	#T_NOINT|T_Z_PRELOAD,TCR(A2)
					; Autovector (#4/ $70)/ Preload/ Pin32count/Prescaler
		
		move.w 	#10,-(sp)	 
					;( 0,5 sec @ E=1.0 MHz & prescaler 32)
		move.w	#80,-(sp)
					; vector #80 - vector adr $140
		jsr		initTimer68230
		rts
***---------------------------------------------------------------


;****************************************************************
****	68681 Timer initializing
;****************************************************************

SetThe68681Timer:
		move.w 	#0,-(sp)	; not used...				
		
		move.w 	#1,-(sp)	 ;( 15 sec @ E=1.2 MHz & prescaler 256)
		move.w	#90,-(sp) 	; vector #90 - vector adr $168
		jsr		initTimer68681
		rts
***---------------------------------------------------------------


***************************************************	
	***  jumptable
***************************************************	
		even
jumptable:
		DC.L	0,p_dumpmem,p_dumpmem,p_dumpmem,p_List,p_Clear,p_PCval  ; 0-6
		DC.L	p_INCPC,p_DECPC,p_showStack,p_showUStack,p_EEP,p_SRAM	; 7-12	
		DC.l	p_runexec,p_load,p_GoToAdr,p_lfc						; 13-16
		DC.l	p_storeLong,p_storeByte,p_storeWord,p_escseq			; 17-20

		even
DELIMS: DC.B	'$ =',0,0
		even

ComTex:	DC.b	'DUMPMEM',0,1,0		;$04
		even
		dc.w	ECX
		DC.B	'DM',0,2,0			;$08
		even
		dc.w	ECX
		DC.B	'DM',0,3,0			;$0C
		even
		dc.w	ECX
		DC.B	'CLEAR',0,5,0		;$14
		even
		dc.w	ECX
		DC.B	'CM',0,5,0			;$14
		even
		dc.w	ECX
		DC.B	'$',0,6,0			;$18
		even
		dc.w	ECX
		DC.B	'PC',0,6,0			;$18
		even
		dc.w	ECX
		DC.B	'++',0,7,0			;$1C
		even
		dc.w	ECX
		DC.B	'--',0,8,0			;$20
		even
		dc.w	ECX
		DC.B	'STACK',0,9,0			;$24
		even
		dc.w	ECX
		DC.B	'USTACK',0,10,0			;$28
		even
		dc.w	ECX
		DC.B	'EEP',0,11,0			;$28
		even
		dc.w	ECX
		DC.B	'SRAM',0,12,0			;$2C
		even
		dc.w	ECX
		DC.B	'RUN',0,13,0			;$2C
		even
		dc.w	ECX
		DC.B	'RUN',0,13,0			;$2C
		even
		dc.w	ECX
		DC.B	'EXEC',0,13,0			;$2C
		even
		dc.w	ECX
		DC.B	'LOAD',0,14,0			;$2C
		even
		dc.w	ECX
		DC.B	'LD',0,14,0			;$2C
		even
		dc.w	ECX
		DC.B	'GO',0,15,0			;$38
		even
		dc.w	ECX
		DC.B	'LFC',0,16,0			;$38
		even
		dc.w	ECX
		DC.B	'L.',0,17,0			;$38
		even
		dc.w	ECX
		DC.B	'B.',0,18,0			;$30
		even
		dc.w	ECX
		DC.B	'W.',0,19,0			;$34
		even
		dc.w	ECX
		DC.B	'escseq',0,20,0			;$3C
		even
		dc.w	ECX
		DC.w	ECM


p_dumpmem:
		move.l	PCval,D0
		move.l	D0,dumpStack_LO
		add.l	D4,D0
		move.l	D0,dumpStack_HI
		trap #1
		
		rts
p_List:


		rts
p_Clear:
		move.l	A0,-(sp)
		move.l	PCval,A0
		move.l	D4,D0
.fillzero:
		move.l	#$1,(A0)+
		subi.l	#4,D0
		bge	 .fillzero

		move.l	(sp)+,A0	
		rts

	***	Set PCval with value in D3	
p_PCval:
		bclr.l	#0,D4	
		move.l	D4,PCval		; set new value	
		rts


	***	Increase PCval with one word
p_INCPC:		
		move.l	PCval,D0
		cmpi.l	#0,D4
		beq.s	.incval
		subq	#2,D0
		add.l	D4,D0
.incval:
		addq	#2,D0
		bclr.l	#0,D0
		move.l	D0,PCval
		rts

	***	Decrease PCval with one word
p_DECPC:		
		move.l	PCval,D0
		cmpi.l	#0,D4
		beq.s	.decval
		addq	#2,D0
		sub.l	D4,D0
.decval:
		subq	#2,D0
		bclr.l	#0,D0
		move.l	D0,PCval
		rts

p_showStack:

		move.l	#startsp-$80,dumpStack_LO
		move.l	#startsp+$40,dumpStack_HI
		trap #1

		rts

p_showUStack:		;the user stack

		move.l	#startusp-$80,dumpStack_LO
		move.l	#startusp+$40,dumpStack_HI
		trap #1

		rts

p_EEP:	
		move.l	#$A00000,PCval
		move.l	#$A00000,dumpStack_LO
		move.l	#$A00100,dumpStack_HI
		trap #1
		rts

p_SRAM:
		move.l	#$00,PCval
		move.l	#$00,dumpStack_LO
		move.l	#$100,dumpStack_HI
		trap #1
		rts
p_runexec:
		lea		startusp,A0
		move.l	A0,USP
		move.w	#0,-(sp)
		move.l	D4,-(sp)			; program address in D4	
		move.w	#$100,-(sp)			; init val SR
		move.w	#$00,IO_LS373		; LCD shows jump to testprogram

		rte						; jump to program 
	***	; program should return with trap #0	

p_load:
	***	;read motorola S-rec files from terminal

		trap	#7

		rts

p_lfc:
	*** ; Read commands from C-kermit (wermit)
		jmp Kermit_start

p_GoToAdr:
	***	;read motorola S-rec files from terminal and jump to startaddress

		trap	#7
		move.l	SREC_GoAdr,D4
		bra.s	p_runexec
		; rts


p_storeByte:
		move.l	PCval,A1
		move.b	D0,(A1)
		rts

p_storeWord:
		move.l	PCval,A1
		move.w	D0,(A1)
		rts

p_storeLong:
		move.l	PCval,A1
		move.l	D0,(A1)
		rts

p_escseq:
		trap #9



************************************************************************

		; section .sec_HD44780_Init
		

		xdef Init_HC44780_start, clearLCD

		align 3

************************************************************************
		; Set up 68230 Timers : Requires Base68230 in A2
		xdef Main_68230_init

Main_68230_init::
		lea Base68230,A2
		clr.l D0
		move.b D0,(PGCR,A2)
		move.b D0,(PSRR,A2)
		move.b #%00000000,D0
		move.b	D0,(PACR,A2)
		move.b #$00,(PBCR,A2)

		move.b #$FF,D0
		move.b D0,(PADDR,A2)		port A output
		move.b D0,(PBDDR,A2)		port B output	


		move.b 	#$33,D0
		; move.b	#$AA,(PADR,A2)
		; move.b	#$AA,(PBDR,A2)
        rts
;************************************************************************

;************************************************************************
		; Set up 68230 Timers : Requires Base68230 in A2

Main_68230_Timer_init::	

		move.b 	#T_INT_V|T_TIN_P|T_Z_PRELOAD,TCR(A2)	; Vector int (#4/ $70)/ Preload/ Pin32count/Prescaler
		;move.b 	#T_NOINT|T_Z_PRELOAD,TCR(A2)	; Autovector (#4/ $70)/ Preload/ Pin32count/Prescaler
		move.l 	#$7A12,D0					;E=1.0 MHz/ prescaler 32 ->31250  ($7A12)
 		movep.l D0,Tpreload(A2)
		move.b	#80,TIVR(A2)		; vector #80 - vector adr $140
		move.w	#$2200,SR			; vector level 2
		rts
;************************************************************************
		align 	3
;************************************************************************
		; Set up 68681 circuit, including Timer : Requires Base68681 in A4
;	68681 INIT
		xdef Main_UART_68681_init
Main_UART_68681_init::

		;COUNTER PRESET
		; circuit 74HC4060; 16384(p3),8192(p2),4096(p1),
		;					1024(p15),512(p15),256(p14),
		;					128(p6),64(p4),32(p5),16(p7)
		;					3686400/64 -> $E100
		;					800000/64 -> $30D4
		move.b	#$30,CTUR(A4)
		move.b	#$D4,CTLR(A4)
		move.b	BRG(A4),D0				; BRG test mode
		move.b	#%000<<4,ACR(A4)		; clock of channel A transmitter								; 0 -> external IP2 clock

		move.b	#10<<4,CRA(A4)					; Reset receiver 		
		move.b	#11<<4,CRA(A4)					; Reset transmitter		
		move.b	#1<<4,CRA(A4)					; Reset MR pointer		
		move.b	#B_115_2,CSRA(A4)		; Receiver and transmitter Baud rate
		; move.b	#B_57_6,CSRA(A4)		; Receiver and transmitter Baud rate
		; move.b	#B_9600,CSRA(A4)		; Receiver and transmitter Baud rate
		move.b	#NO_PARITY|P_B8,MR1A(A4)
		move.b	#CM_NORMAL|STOP_B1,MR2A(A4)
		** init sequence with RTS/CTS flow
		; move.b	#RxRTS|NO_PARITY|P_B8,MR1A(A4)
		; move.b	#CTS_E|CM_NORMAL|STOP_B1,MR2A(A4)

		move.b	#10<<4,CRB(A4)					; Reset receiver 		
		move.b	#11<<4,CRB(A4)					; Reset transmitter		
		move.b	#1<<4,CRB(A4)					; Reset MR pointer
		move.b	#B_115_2,CSRB(A4)		; Receiver and transmitter Baud rate
		; move.b	#B_9600,CSRB(A4)		; Receiver and transmitter Baud rate
		move.b	#NO_PARITY|P_B8,MR1B(A4)
		move.b	#CM_NORMAL|STOP_B1,MR2B(A4)
		; Interrupt master
		move.b 	#0,IMR(A4)			; allow interrupt on 'Counter ready'


		;Interrupt vector
		move.b	#90,DUA_IVR(A4)			; interrupt vector no 90

		;OUTPUTS
		move.b	#%00000100,OPCR(A4)	;
		move.b	#TxENABLE|RxENABLE,CRA(A4)
		move.b	#TxENABLE|RxENABLE,CRB(A4)
		rts

		;Sets the speed (baud rate) for the serial communication. 
		;Supported baud rates are 300, 600, 1200, 2400, 4800, 9600,
		; 14400, 19200, 28800, 31250, 38400, 57600, and 115200.
;********************************************************************		



;****************************************************************

		;****************************************************************
		;	clear IL9320
		;  Parameters passed: color background in D0 (Y-14; G-15; B-16)

		; ADAFruit GFX colors   B + G<<5 + R<<11
		; R 5 bits; G 6 bits; B 5 bits
		; move.l	#'B',-(sp)
		; jsr		clearIL9320

		xdef	clearIL9320
clearIL9320:

; .colorix_h 	= 8		
; .colorix_l 	= 10		
; .param2		= 2
		
		link 	A6,#0
		movem.l	D0-D1,-(sp)

		move.w	#'C',D1				; 'C' for clear screen (fill background)
		jsr 	_chr_t_IL9320		; send first x-coord
		clr.l	D1
		jsr 	_chr_t_IL9320		; send 2nd y-coord
		jsr 	_chr_t_IL9320		; send 2nd y-coord

		
		move.b	#CHAR_LF,D1
		jsr		_chr_t_IL9320

		movem.l	(sp)+,D0-D1
		unlk	A6
		; move.l	(sp),(.param2,sp)
		; addq.l	#.param2,sp

		rts		

;****************************************************************

;****************************************************************

		;****************************************************************
		;	set foreground color IL9320
		;  Parameters passed: color foreround on stack 3 bytes (Y-14; G-15; B-16)

		; ADAFruit GFX colors   B + G<<5 + R<<11

		xdef	setfgcIL9320
setfgcIL9320:

		
		link 	A6,#0
		movem.l	D0-D1,-(sp)

		move.l	#'F',D1				; 'F' for foreground
common1:
.param2		= 2
.colorix 	= 8		
		jsr 	_chr_t_IL9320		; Send forground command 'F', 'B' for background

		move.w	(.colorix,A6),D1	;  color index to D1
		lsr.w	#8,D1
		jsr 	_chr_t_IL9320		; send first x-coord (msb of colorindex)

		move.w	(.colorix,A6),D1	;  color index to D1
		andi.b	#$0F,D1
		jsr 	_chr_t_IL9320		; send 2nd y-coord (lsb of colorindex)

		
		move.b	#CHAR_LF,D1
		jsr		_chr_t_IL9320

		movem.l	(sp)+,D0-D1
		unlk	A6
		move.l	(sp),(.param2,sp)
		addq.l	#.param2,sp

		rts		

;****************************************************************


;****************************************************************

		;****************************************************************
		;	set foreground color IL9320
		;  Parameters passed: color foreround on stack 3 bytes (Y-14; G-15; B-16)

		; ADAFruit GFX colors   B + G<<5 + R<<11

		xdef	setbgcIL9320
setbgcIL9320:

.colorix 	= 8		
.param2		= 2
		
		link 	A6,#0
		movem.l	D0-D1,-(sp)

		move.l	#'B',D1				; 'B' for background
		bra		common1
		
;****************************************************************


;********************************************************************		
		;**************************************************************
		; putstr(S)
        ; parameters passed; cursor pos in 2 words; string address ('0' temrin)
        ; Parameters returned; None
	
		; move.w	#$04,-(sp)		; coumn - xpos
		; move.w	#$05,-(sp)		; row  - ypos
		; pea		hexdigits
		; jsr		stroutIL9320
	
		align 3
		xdef stroutIL9320,_chr_t_IL9320

stroutIL9320::				; check cursor pos and print string to IL9320 display
w_pix	equ	10				; pixel width of char
h_pix 	equ	22				; pixel height of char
.col_addr:	=14
.row_addr:	=12
.str_addr:	=8
;.param8:	= 8
			; example:

			; row and col pushed on stack  <col><row>.w in word data.
		link 	A6,#0
		movem.l	D0-D1/A1-A4,-(sp)

		move.l	#0,D1
		jsr 	_chr_t_IL9320		; no reset of screen

		clr.l	D0
		move.w	(.col_addr,A6),D1	; D0=  col	
		mulu	#w_pix,D1			; D0 height pixels (y-coord)
		jsr 	_chr_t_IL9320		; send first x-coord
		
		move.w	(.row_addr,A6),D1	; D1= row 
		mulu	#h_pix,D1			; D1 width pixels (x-coord)
		jsr 	_chr_t_IL9320		; send 2nd y-coord

		move.l	(.str_addr,A6),A1

txrx_68681:
loop$:							; send chars to 68681 port A
		move.b	(A1)+,D1

		CMPI.B	#0,D1				;sista tecken?			
		BEQ 	nxtlf2$
		jsr 	_chr_t_IL9320		; send char
		bra		loop$
nxtlf2$:
.param8:	= 8
		move.b	#$20,D1
		moveq	#4,D0
spaces$:		
		jsr 	_chr_t_IL9320		; send 'space' to 68681
		jsr 	_chr_t_IL9320		; send 'space' to 68681
		dbra	D0,spaces$

		move.b	#CHAR_LF,D1
		jsr 	_chr_t_IL9320		; send LF to 68681


		movem.l	(sp)+,D0-D1/A1-A4

		unlk	A6
		move.l	(sp),(.param8,sp)
		addq.l	#.param8,sp

		rts		

_chr_t_IL9320:	; send one char in (A1) to 68681 port A
		BTST.B	#TxRDY,SRB(A4)		;Test TxRDY
		BEQ.s	_chr_t_IL9320
		move.b	D1,THRB(A4)			; send char to 68681
		; move.l	#D_1ms,-(sp)			; timeconstant for Timer
		; bsr 	Wait_delay_time		; set timer and wait. 0.1 ms
		rts


;****************************************************************









;****************************************************************

		;****************************************************************
		;	stroutlcd IL9320
		;  Parameters passed: Address in A0, size in D0	
		xdef	stroutlcd
stroutlcd:

		movem.l	D0-D1/A1-A2,-(sp)


		
		movem.l	(sp)+,D0-D1/A1-A2
		rts

_rowoffset:             dc.b 0,$40,$14,$54

;********************************************************************		
		
		align 3
		xdef setcursor
setcursor44780::					; old 'setcursor' for 44780display

.row_addr:	= 8
.param4:	= 4

			; row and col pushed on stack  <col><row>.w in word data.
		link 	A6,#0
		movem.l	D0-D1/A2-A4,-(sp)

		clr.l	D1
		move.l	(.row_addr,A6),D0	; D0= row & col
		move.b	D0,D1				;set lowbyte to D1, D1-> column
		lsr.w	#8,D0				;set low byte to row, D0-> row

		lea		_rowoffset,A2		; point to _rowoffset:
		move.b	(A2,D0),D0			; get rowoffset		
		add.b	D1,D0				; add the #column. D0 is final adress/value	
		bset 	#7,D0				; LCD_SETDDRAMADDR  command
		bsr		write_command		;send command to A2004		
		
		move.l	#D_1ms,-(sp)		; extra delay for cursor
		bsr 	Wait_delay_time		; set timer and wait. 1 ms

		movem.l	(sp)+,D0-D1/A2-A4

		unlk	A6
		move.l	(sp),(.param4,sp)
		addq.l	#.param4,sp

		rts		


;****************************************************************
		align 3
write_command:
		xdef write_command

		; D0 contains value
		; D0 contain 8 bits data ; command -> rs = 0

		movem.l	D0/A3,-(sp)
        clr.l	D1  		; indicate rs pin =0 (pin 5)
        bsr 	send
		
		movem.l	(sp)+,D0/A3
		rts

;****************************************************************

		;****************************************************************
		;	stroutlcd 44780
		;  Parameters passed: Address in A1, size in D0	
		xdef	stroutlcd
stroutlcd44780:

		move.l	D0,-(sp)
		move.l	D0,D4
		subq.w	#1,D4
1$:		move.b	(A0)+,D0
		jsr		write_data
		dbra	D4,1$
		move.l	(sp)+,D0
		rts




;****************************************************************
        align 3
write_data:		; D0-value; D1-mode 1(data), 0(command)
		xdef	write_data

		movem.l	D0/A3,-(sp)
        clr.l	D1  		
		bset.l	#5,D1	; D0 contains value, D1 set 1 (data)
                        ; indicate rs pin =1 (pin 5 set)
	    bsr send
		movem.l	(sp)+,D0/A3
        rts

;****************************************************************
        align 3
send:                 ; D0-value; D1-mode 1(data), 0(command)
        clr.l	D3
		move.b	D0,D2       ;copy D0 to D2
        lsr.b	#4,D0
        or.b	D1,D0 		;add eventually pin 5 (RS)
        bsr 	write4bits
        move.b	D2,D0		;restore value to D0
        and.b	#$0F,D0
        or.b	D1,D0 		;add eventually pin 5 (RS)
       	bsr		write4bits
   		
		move.l	#D_100us,-(sp)	; timeconstant for Timer
		bsr 	Wait_delay_time		; set timer and wait. 0.1 ms

        rts

;****************************************************************

;****************************************************************
		align 3
write4bits:   
		; D0 contains value		
		;****************************************************************
		; pulse the enable pin... (pin 4)
        ; D0 contains data...

		lea 	IO_LS373,A3
		move.b 	#LCD_E,(1,A3)	;send to command address rs pin =0,  set E pulse high
		or.b	#LCD_E,D0
		move.b 	D0,(1,A3)		;send to command address rs pin =0,  set E pulse high
		nop
		nop
		nop
		and.b	#~LCD_E,D0
		move.b 	D0,(1,A3)		;send to command address rs pin =0,  set E pulse low

		nop
		nop
		nop
		move.b 	#$00,(1,A3)		;send to command address rs pin =0,  set E pulse low

        rts

;****************************************************************

******************************************************
		;*************************************************
		; TIMER settings for delays
		;*************************************************

		;No prescaler and E signal (p.20) as Tin
		;1000 khZ / #1000000 / sec   ( @ descaler $31250)
		;					Descaler(32)
		;50 ms: #50000  ($C350)		#1562 ($61A)	
		;20 ms: #20000  ($4E20)		#625  ($271)
		;10 ms: #10000  ($2710)		#312  ($138)
		; 5 ms: #5000   ($1388)		#156  ($9C)
		; 4 ms: #4000   ($FA0)
		; 3 ms: #3000   ($BB8)
		; 2 ms: #2000   ($7D0)
		; 1 ms: #1000   ($3E8)
		;500 µs: #500   ($1F4)
		;200 µs: #200   ($C8)
		;100 µs: #100   ($64)

		align 2
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;		WAit n seconds, n stored in stack
;		
		xdef wait_n_secs
wait_n_secs:
.secs			equ		8
.param2:		equ		2

		link a6,#0
		movem.l	d0-d1/a0-a2,-(sp)	
		clr.l	d1
		move.w	(.secs,a6),d1
		lea Base68230,A2

		;++++++++ Set timer
		move.b #T_NOINT|T_Z_PRELOAD|T_TIN_P,TCR(A2)	; prescaler used (bit1)
		move.l #25000,D0		; E=0.8MHz / 32 -> 25 khz  #$7A12
		mulu.w	d1,d0			; d0 resulting seconds
		movep.l D0,Tpreload(A2)
		BSET	#0,TCR(A2)

.Tpoll:
		BTST #0,(TSR,A2)
		;move.b (CRM,A2),D1
		;move.b	D1,(PADR,A2)
		BEQ.s  	.Tpoll
		BCLR 	#0,TSR(A2)


		movem.l	(sp)+,d0-d1/a0-a2
		unlk	a6
		move.l	(sp),(.param2,sp)
		adda.w	#.param2,sp
		rts		

;======================================================


		align 3
Tconstant = 8
Wait_delay_time:

		xdef Wait_delay_time
		;  time constant left on the stack
		;  move data to direct addres  ~2µs/cycle -> 500/ms.
		link 	A6,#0
		move.l	D0,-(sp)

		move.l	(Tconstant,A6),D0
.T_poll:
		move.l	#$DC9876,$F8000
		nop
		dbra	D0,.T_poll

		move.l	(sp)+,D0
		unlk	A6
		move.l	(sp),(4,sp)
		addq.l	#4,sp
		rts

******************************************************************
****  String values; to be copied to HEAP space
******************************************************************
; scratchbuf::
; Cstr1::		
; Cstr2::		
; InText::	
; inputqueue::
; outputqueue::
; suspend_for_input::
; suspend_for_output::
; xoff_typed::
; decbuf:: 

		xdef	prep_key_term_buf,prep_HEAP_SPACE
prep_key_term_buf::

		; fill FC000 - FC3FF with 0
		movem.l	D1/A1,-(sp)

		lea		keyBufPtr,A1
		move.l	#$80,D1		; define 4k ram area
.10:		
		move.l	#0,(A1)+
		DBRA	D1,.10
		move.l	#keyInputBuffer,keyBufPtr ; point to the buffer for key input
		move.l	#termOutputBuffer,termBufPtr ; point to the buffer for terminal output

		movem.l	(sp)+,D1/A1
		rts


prep_HEAP_SPACE::

		; fill FC000 - FC3FF with 0
		movem.l	D1/A1,-(sp)

		lea		_HEAP_SPACE,A1
		move.l	#$400,D1		; define 4k ram area
.10:		
		move.l	#0,(A1)+
		DBRA	D1,.10
		movem.l	(sp)+,D1/A1
		rts


STRING_HEAP::

; *************************************************
; **		Writeable HEAP area				***
; **********************************************

; scratchbuf::
; 		DS.b	256
; 		even			
; Cstr1::		
; 		ds.b	80
; 		even
; Cstr2::		
; 		ds.b	80

; 		even
; InText::	
; 		ds.b	256


; inputqueue::
; 		ds.w 	1				; front index
; 		ds.w 	1				; rear index
; 		ds.w	1				; count
; 		ds.w	1				; size
; 		ds.l	queuesize		; input buffer
; 		even
; outputqueue::
; 		ds.w 	1				; front index
; 		ds.w 	1				; rear index
; 		ds.w	1				; count
; 		ds.w	1				; size
; 		ds.l	queuesize		; output buffer
; 		even
; suspend_for_input::
; 		ds.w	1				; input queue empty

; suspend_for_output::
; 		ds.w	1				; output queue full

; xoff_typed::
; 		ds.w	1				; handle XON/XOFF

; 		even
; decbuf:: 
; 		ds.b	12								




STRING_HEAP_ORIGIN::

*************************************************
**		Readable HEAP area				***
**********************************************

		even
hexdigits:: 						
		dc.b	'0123456789ABCDEF',0


		even	
start_msg::						
		dc.b	'Proc. start !',$0D,'password:',0

		even	
test_msg::						
		dc.b	'Try Again ::: ->_',0

		even	
c_err_msg::						
		dc.b	' Unknown Command!',0

		even	
msg_valuerr::						
		dc.b	' Value ERROR (non hex)!',0
		
		even	
S0_err_msg::						
		dc.b	' ERROR: No S0 record detected!',0
		
		even	
SREC_chsum_err_msg::						
		dc.b	' ERROR: SREC checksum mismatch!',0
		
		even
run_msg::						
		dc.b	' Running ..... ->>_',0
		
		even	
stop_msg::						
		dc.b	'STOP ::: -> ',0

		even	
test_msg2::						
		dc.b	'***********',0
		even
test_msg3::						
		dc.b	'1 minute finish *',0
		even
test_msg4::
		dc.b	'Welcome to the system (LB68K)    ',0
		even
SYS_msg1::						
		dc.b	'System (LB68K) - INIT   ',0
		even
SYS_msg2::						
		dc.b	'68010, 10MHz - 68230 - 68681 - (8536)   ',0
		even
SYS_msg3::						
		dc.b	'1Mb RAM ASC64008, 1MbFLASH SST39SF040 ',0
		even
test_msg5::
		dc.b	'input sequence : ->   ',0
		dc.w	$AAEE
bus_message::		
		dc.b	'  BUS ERROR !',0
		even
adr_message::	
		dc.b	'  ADDRESS ERROR !',0
		even
ill_message::	
		dc.b	'  ILLEGAL INSTRUCTION !',0
		even
zdv_message::	
		dc.b	'  ZERO DIVIDE !',0
		even
chk_message::	
		dc.b	'  CHK INSTRUCTION !',0
		even
tpv_message::	
		dc.b	'  TRAPV INSTRUCTION !',0
		even
priv_message::	
		dc.b	'  PRIVILEGE VIOLATION !',0
		even
trace_message::	
		dc.b	'  TRACE !',0
		even
aln_message::	
		dc.b	'  A-LINE EXCEPTION !',0
		even
fln_message::	
		dc.b	'  F-LINE EXCEPTION !',0
		even
ferr_message::	
		dc.b	'  FORMAT ERROR !',0
		even
uiv_message::	
		dc.b	'  UNINITIALIZED INTERRUPT VECTOR !',0
		even
spu_message::	
		dc.b	'  SPURIUOUS INTERRUPT !',0
		even
une_message::	
		dc.b	'  UNDEFINED EXCEPTION !',0
		even
star_row_msg::	
		dc.b	'*_*_*_*_*_*_*_*_*_*_*_*',0
		even
PC_msg::
		dc.b	'Prog Counter  = ',0,0
		even
VO_msg::
		dc.b	'Vector offset = ',0,0

		even
D0_msg::
		dc.b	'D0= ',0,0

		even
FA_msg::
		dc.b	'Fault Addresss = ',0,0
		even

SpecStat_msg::
		dc.b	'Special Status = ',0,0
		even

CPUStat_msg::
		dc.b	'CPU Status = ',0,0
		even

Stack_msg::
		dc.b	'Stack (A7) = ',0,0
		even

UStack_msg::
		dc.b	'User Stack (A7) = ',0,0

		even
NSP_msg::
		dc.b	'NMI_SP= ',0,0

		even
IRQ4SP_msg::
		dc.b	'V_SP= ',0,0
		even
		even
Vector_msg::
		dc.b	'Int.Vec.= ',0,0
		even
		even
exceptmsg::
		dc.b	'unexpected exception at ',0,0
		even
NMI_msg::
		dc.b	'NMI...IRQ7! ',0,0
		
		even
ENTer_msg::
		dc.b	'Press Ent to continue ',0,0
		even
cmdprompt::
		dc.b	'===>>_: ',0
		even
TIVR_IRQ4_msg::
		dc.b	'Timer IRQ4->: ',0,0

		even
DUART_IRQ_msg::
		dc.b	'DUART IRQ.-- ',0,0

		even
PTM_IRQ_msg::
		dc.b	'------- IRQ.-- ',0,0

		even
SOFT_IRQ_msg::
		dc.b	'SOFT(#2) IRQ.-- ',0,0


		even
trap1_msg::
		dc.b	'Trap1! ->:-). ',0,0

		even
pwstring::						
		dc.b	'abc123',0

		even
INIstring::						
		dc.b	'---> INIT <---',0
		even
registers::
		dc.b	' PC= ',0,0,0,' SR= ',0,0,0,' SSP= ',0,0,' USP= ',0,0
		dc.b	' D0= ',0,0,0,' D1= ',0,0,0,' D2= ',0,0,0,' D3= ',0,0,0,' D4= ',0,0,0,' D5= ',0,0,0,' D6= ',0,0,0,' D7= ',0,0,0
		dc.b	' A0= ',0,0,0,' A1= ',0,0,0,' A2= ',0,0,0,' A3= ',0,0,0,' A4= ',0,0,0,' A5= ',0,0,0,' A6= ',0,0,0
		dc.b	' opcode next word next instruction = ',0,0
		even
CMD_str::
		dc.b	': ==>>',0,0


STRING_HEAP_ORIGIN_END:



******************************************************

		end

******************************************************
; ;	Unused Modules for HC44780
; Init_HC44780_start::	
; 		;//put the LCD into 4 bit or 8 bit mode
; 		;// this is according to the hitachi HD44780 datasheet
; 		;// figure 24, pg 46

; 		move.l	#D_50ms,-(sp)	; timeconstant for Timer
; 		bsr 	Wait_delay_time		; resulting wating 50 ms

;   		move.b	#03,D0	
; 		bsr 	write4bits
; 		move.l	#D_5ms,-(sp)			; timeconstant for Timer
; 		bsr 	Wait_delay_time		; set timer and wait. 5 ms

;   		move.b	#03,D0	
; 		bsr 	write4bits
; 		move.l	#D_500us,-(sp)			; timeconstant for Timer
; 		bsr 	Wait_delay_time		; set timer and wait. 0.5 ms

;   		move.b	#03,D0	
; 		bsr 	write4bits
; 		move.l	#D_100us,-(sp)		; timeconstant for Timer
; 		bsr 	Wait_delay_time		; set timer and wait. 0.1 ms

;   		move.b	#02,D0	
; 		bsr 	write4bits
; 		move.l	#D_100us,-(sp)		; timeconstant for Timer
; 		bsr 	Wait_delay_time		; set timer and wait. 0.1 ms

; 		;// Set # lines, font size, etc.
; 		move.b	#LCD_FUNCTIONSET|LCD_4BITMODE|LCD_2LINE|LCD_5x8DOTS,D0	;$28
; 		bsr 	write_command

; 		; turn the display on with  cursor and blinking default
; 		move.b 	#LCD_DISPLAYCONTROL|LCD_DISPLAYON|LCD_CURSORON|LCD_BLINKON,D0  	; = $0C
; 		bsr 	write_command

; 		;// set the entry mode
; 		;// Initialize to default text direction (for romance languages)
; 		move.b	#LCD_ENTRYMODESET|LCD_ENTRYLEFT,D0    					; =$06
; 		bsr 	write_command

; clearLCD::
; 		; Clear display....
; 		move.b	#LCD_CLEARDISPLAY,D0							; =$01
; 		bsr 	write_command
; 		move.l	#D_20ms,-(sp)			; timeconstant for Timer
; 		bsr 	Wait_delay_time		; set timer and wait. 0.1 ms

; 		rts

