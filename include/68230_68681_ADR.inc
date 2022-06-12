
CHAR_LF:			equ $0A
CHAR_CR:			equ	$0D
CHAR_tab:			equ	$09
lowcase_A:			equ	'a'
lowcase_Z:			equ	'z'
ctrl_Z:				equ	$1A
ctrl_E:				equ	$05
ctrl_C:				equ	$03
ctrl_H:				equ	$08
ctrl_I:				equ	$09
ctrl_J:				equ	$0A
ctrl_K:				equ	$0B
ctrl_L:				equ	$0C
ctrl_M:				equ	$0D

Null:				equ  0
STX:	equ		02
ETX:	equ		03
EOT:	equ		04			; slutmarkering
ENQ:	equ		05
ACK:	equ		06
DLE:	equ		$10
DC1:	equ		$11
DC2:	equ		$12
DC3:	equ		$13
DC4:	equ		$14
NAK:	equ		$15
SYN:	equ		$16
ETB:	equ		$17
ECM:	equ		$EE			; end of command list
ECX:	equ		$FE			; COMMAND 	delimiter

queuesize:			equ	 $60			;buffer up to sixteen chars

; PIO 86320  constants


PGCR	equ 	$00		; Port General Control Register
PSRR	equ		$02		; Port Service Request Register
PADDR	equ		$04		; Port A Data Direction Register
PBDDR	equ		$06		; Port B Data Direction Register
PCDDR	equ		$08		; Port C Data Direction Register
PIVR	equ		$0A		; Port Interrupt Vector Register
PACR	equ		$0C		; Port A Control Register
PBCR	equ		$0E		; Port B Control Register
PADR	equ		$10		; Port A Data Register
PBDR	equ		$12		; Port B Data Register
PAAR	equ		$14		; Port A Alternate Register
PBAR	equ		$16		; Port B Alternate Register
PCDR	equ		$18		; Port C Data Register
PSR	    equ		$1A		; Port Status Register
TCR	    equ		$20		; Timer Control Register
TIVR	equ		$22		; Timer Interrupt Vector Register	
Tpreload   equ  $24     ; MOVEP preload address
CPRH	equ		$26		; Counter Preload Register High
CPRM	equ		$28		; Counter Preload Register Mid
CPRL	equ		$2A		; Counter Preload Register Low		
CRH		equ		$2E		; Count Register High
CRM		equ		$30		; Count Register Mid
CRL		equ		$32		; Count Register Low
TSR		equ		$34		; Timer Status Register


TIM_EN	equ		1<<0  	; Timer Enable const.  
T_CLK_TIN_P	equ		1<<1	; Timer: Clock count/ Tin enable/ Prescaler.
T_TIN_P		equ		2<<1	; Timer: Tin/PC2 counter/ Prescaler. pin32 with prescaler 32	
T_TIN		equ		3<<1	; Timer: Tin/PC2 counter/ NO Prescaler.	
;----------------------------------------------------------
T_ZDC		equ		1<<4	; Timer Zero Detect Control; The counter rolls over on zero detect, then continues counting.
							; else counter is loaded from the counter preload register, then resumes counting.
T_Z_PRELOAD	equ		0		; Use preload register during zero count down
;------------------------------------------------------------
T_SQWAVE	equ		2<<5	; PC3/TOUT carries the TOUT (square wave)							
T_NOINT		equ		4<<5	; No interrup gen.
T_INT_V		equ		5<<5	; TOUT= INT. / the pin is low when the timer ZDS status bit is one.\
							; TIACK = 	supports vectored timer interrupts.
T_INT_A		equ		7<<5	; TOUT= INT. / the pin is low when the timer ZDS status bit is one. \


							; TIACK = 	supports autovectored timer interrupts.

; DUART 68681 constants
;								; 	READ (R/WN=1)					WRITE (R/WN=0)
MR1A	equ 	$00			
MR2A	equ 	$00			;	Mode Register A(MR1A, MR2A) 	Mode Register A(MR1A, MR2A) 
SRA		equ		$02		
CSRA	equ		$02			;	Status Register A (SRA)			Clock Select Register A (CSRA)
BRG		equ		$04
CRA		equ		$04			;	BRG test						Command Register A (CRA)
RHRA	equ		$06
THRA	equ		$06			;	Rx Holding Register A (RHRA)	Tx Holding Register A (THRA)		[alt. RBA/ TBA]
ACR		equ		$08
IPCR	equ		$08			;	Input Port Change Register (IPCR)Aux. Control Register (ACR)
ISR		equ		$0A
IMR		equ		$0A			;	Interrupt Status Register (ISR)	Interrupt Mask Register (IMR)
CTU		equ		$0C
CTUR	equ		$0C			;	Counter/Timer Upper Value (CTU)	C/T Upper Preset Value (CRUR)		[alt. CUR/ CTUR]
CTL		equ		$0E
CTLR	equ		$0E			;	Counter/Timer Lower Value (CLR)	C/T Lower Preset Value (CTLR)
MR1B	equ		$10
MR2B	equ		$10			;	Mode Register B (MR1B, MR2B)	Mode Register B (MR1B, MR2B)
SRB		equ		$12
CSRB	equ		$12			;	Status Register B (SRB)			Clock Select Register B (CSRB)
CRB		equ		$14			;	1×/16× Test						Command Register B (CRB)
RHRB	equ		$16			
THRB	equ		$16			;	Rx Holding Register B (RHRB)	Tx Holding Register B (THRB)		[alt. RBB/ TBB]
DUA_IVR	equ		$18			;	Interrupt Vector Register (IVR)	Interrupt Vector Register (IVR)
IP		equ		$1A
OPCR	equ		$1A			;	Input Port	(IP)				Output Port Conf. Register (OPCR)
STAC	equ		$1C
SOPR	equ		$1C			;	Start Counter Command			Set Output Port Bits Command		[alt. STAC/ OPR]
STOC	equ		$1E
ROPR	equ		$1E			;	Stop Counter Command			Reset Output Port Bits Command		[alt. STOC/ OPR??]

; MR1 Codes
RxRTS	equ		1<<7
RxINT_RxRDY	EQU	0
RxINT_FFULL	EQU	1<<6
E_CHAR		equ	0
E_BLOCK		equ 1<<5
PARITY		equ	0
F_PARITY	equ	1<<3
NO_PARITY	equ	2<<3
M_PARITY	equ	3<<3		; Multidrop Mode
P_EVEN		equ	0
P_ODD		equ	1<<2
P_B5		equ 0
P_B6		equ 1
P_B7		equ 2
P_B8		equ 3
; MR2 Codes
CM_NORMAL	equ	0
CM_AUTOECHO	EQU	1<<6
CM_LOCAL	EQU	2<<6
CM_REMOTE	EQU	3<<6
TxRTS		EQU	1<<5
CTS_E		EQU 1<<4
STOP_B1		EQU 7
;CR CODES
B_9600		EQU	$BB
B_19_2		EQU	$33
B_28K		EQU	$44
B_57_6		EQU	$55
B_115_2		EQU	$66	
;CRA Codes
TxDISABLE	EQU	1<<3
TxENABLE	EQU	1<<2
RxDISABLE	EQU	1<<1
RxENABLE	EQU	1<<0
; SRA Codes
E_REC_BR	EQU	1<<7
E_FRAME		EQU	1<<6
E_PARITY	EQU 1<<5
E_OVERRUN	EQU	1<<4
TxEMT		EQU	3			; used by bit operation
TxRDY		EQU	2			; used by bit operation
FFULL 		EQU	1			; used by bit operation
RxRDY		EQU	0			; used by bit operation

YellowBG	equ	'A'
GreenBG		equ 'B'
BlueBG		equ	'C'