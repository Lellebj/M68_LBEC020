

;I/D	= 1:Increment
;I/D	= 0:Decrement
;S	= 1:Accompanies display shift
;S/C	= 1:Display shift
;S/C	= 0:Cursor move
;R/L= 1:Shift to the right
;R/L= 0:Shift to the left
;DL= 1:8 bits, DL = 0: 4 bits
;N= 1:2 lines, N = 0: 1 line
;F = 1:5 × 10 dots, F = 0: 5 × 8 dots
;BF	= 1:Internally operating
;BF	= 0:Instructions acceptable DDRAM: Display data RAM

LCD_CLEARDISPLAY	equ		$01
LCD_RETURNHOME 		equ		$02

;// flags for display entry mode
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Entry mode set 			0 	0 	0 	0 	0 	0 	0 	1 	I/D S 	Sets cursor move direction
;and specifies display shift. These operations are performed during data write and read.
LCD_ENTRYMODESET 	equ		$04
;********************************
LCD_ENTRYRIGHT 		equ		$00
LCD_ENTRYLEFT 		equ		$02
LCD_ENTRYSHIFTINCREMENT 	equ		$01
LCD_ENTRYSHIFTDECREMENT 	equ		$00

;// flags for display on/off control
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Display on/off control 	0 	0 	0 	0 	0 	0 	1 	D 	C 	B 	Sets entire display (D) on/off, 37 μs
;cursor on/off (C), and
;blinking of cursor position character (B).
LCD_DISPLAYCONTROL 	equ		$08
;********************************
LCD_DISPLAYON 		equ		$04
LCD_DISPLAYOFF 		equ		$00
LCD_CURSORON 		equ		$02
LCD_CURSOROFF 		equ		$00
LCD_BLINKON 		equ		$01
LCD_BLINKOFF 		equ		$00

;// flags for display/cursor shift
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Cursor or display shift 	0 	0 	0 	0 	0 	1 	S/C R/L — 	— 	Moves cursor and shifts
;display without changing DDRAM contents.
LCD_CURSORSHIFT 	equ		$10
;********************************
LCD_DISPLAYMOVE 	equ		$08
LCD_CURSORMOVE 		equ		$00
LCD_MOVERIGHT 		equ		$04
LCD_MOVELEFT 		equ		$00

;// flags for function set
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Function set 				0 	0 	0 	0 	1 	DL	 N 	F	— 	— 	Sets interface data length 37 μs
;(DL), number of display lines
;(N), and character font (F).
LCD_FUNCTIONSET 	equ		$20
;*******************************
LCD_8BITMODE 		equ		$10
LCD_4BITMODE 		equ		$00
LCD_2LINE 			equ		$08
LCD_1LINE 			equ		$00
LCD_5x10DOTS 		equ		$04
LCD_5x8DOTS 		equ		$00

;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Set CGRAM address 			0 	0 	0 	1 	ACG ACG ACG ACG ACG ACG Sets CGRAM address.
;CGRAM data is sent and received after this setting.
LCD_SETCGRAMADDR 	equ		$40

;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Set DDRAM address 			0 	0 	1 	ADD ADD ADD ADD ADD ADD ADD Sets DDRAM address.
;DDRAM data is sent and received after this setting.
LCD_SETDDRAMADDR	equ		$80

;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Read busy flag & address 	0 	1 	BF 	AC 	AC 	AC 	AC 	AC 	AC 	AC
;Reads busy flag (BF) indicating internal operation is being performed and 
;reads address counter contents.

;HD44780 pin assignments
LCD_RS  = 1<<5;
LCD_RW  = 1<<1;
LCD_E   = 1<<4;

;Time constants @ E=1MHz
		;*************************************************
		; TIMER settings for delays
		;*************************************************

		;No prescaler and E signal (p.20) as Tin
		;1000 khZ / #1000000 / sec   ( @ descaler $31250)
		;					Descaler(32)
		;0,1s: #100000  ($186A0)		#3125 ($C35)
		;50 ms: #50000  ($C350)		#1562 ($61A)	
		;20 ms: #50000  ($4E20)		#625  ($271)
		;10 ms: #10000  ($2710)		#312  ($138)
		; 5 ms: #5000   ($1388)		#156  ($9C)
		; 4 ms: #4000   ($FA0)
		; 3 ms: #3000   ($BB8)
		; 2 ms: #2000   ($7D0)
		; 1 ms: #1000   ($3E8)
		;500 µs: #500   ($1F4)
		;200 µs: #200   ($C8)
		;100 µs: #100   ($64)
