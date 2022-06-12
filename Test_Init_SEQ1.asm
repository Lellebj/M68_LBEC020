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


	include Include/HC373_HD44780_defs.inc
	include Include/68230_68681_ADR.inc
	include Include/exception_vector_addresses.inc
	include Include/colors_IL9320.inc

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
		section .monitor
****************************************************************
***   Start/Init of prog sequence    		
****************************************************************



		; xref 	setcursor,Main_68230_init,Init_68010_vectors

start::

		move.l	#$1A2B3C4E,D1
		move.l	D1,$100200
		move.l	#$98765432,D2
		move.l	D2,$800100
		move.l	$800100,D3
		move.l	D3,$100210

tl1:	bra  tl1

tl2:


boot_msg:
	dc.b    "\r\n\n"
	dc.b	"##########################################################\r\n"
	dc.b	"The 68EC020 Board is awake 2022\r\n"
	dc.b	"      git: @@GIT_VERSION@@\r\n"
	dc.b	"    build: 2022-06-12_21:51\r\n"
	dc.b	"\r\n"
	dc.b	"I/O library tester.\r\n"
	dc.b	"\0"


	end