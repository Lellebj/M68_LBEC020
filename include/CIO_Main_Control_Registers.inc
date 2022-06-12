;INITIAL CREATION
;THIS PROGRAM INITIALIZES THE THREE COUNTER
;TIMERS IN    THE Z8536 CIO TO GENERATE SQUARE WAVES. THEN LOOPS FOREVER,

CIOC:   EQU     0       		; CIO PORT C
CIOB:   EQU     2  				; CIO PORT B
CIOA:   equ     4  				; CIO PORT A
CIOCTL: equ     6  				; CIO CTRL PORT
;Main Control Registers
;Register Name
_MIC: 		equ		0		;Master Interrupt Control
_MIE		equ		1<<7	; MASTER INTERRUPT ENABLE (MIE)
_DLC		equ		1<<6	; DISABLE LOWER CHAIN (DLC)
_NV			equ		1<<5	;NO VECTOR (NV)
_PAVIS		equ		1<<4	;PORT A VECTOR INCLUDES STATUS (PA VIS)
_PBVIS		equ		1<<3	;PORT B VECTOR INCLUDES STATUS (PB VIS)
_CTVIS		equ		1<<2	;COUNTER/TIMERS VECTOR INCLUDES STATUS (CT VIS)
_RJA		equ		1<<1	;RIGHT JUSTIFIED ADDRESSES  0 = SHIFT LEFT (A0 FROM AD1) 1 = RIGHT JUSTIFY (A0 FROM AD0)
_RESET		equ		1<<0	;RESET
;*****************************************************************************
_MCC		equ 	1		;Master Configuration Control
_PBE		equ		1<<7	;PORT B ENABLE (PBE)
_CT1E		equ		1<<6	;COUNTER/TIMER 1 ENABLE (CT1E)
_CT2E		equ		1<<5	;COUNTER/TIMER 2 ENABLE (CT2E)
_CT3E		equ		1<<4	;PORT C AND COUNTER/TIMER 3 ENABLE (PCE AND CT3E)
_PCE		equ		1<<4	;PORT C AND COUNTER/TIMER 3 ENABLE (PCE AND CT3E)
_PLC		equ		1<<3	;PORT LINK CONTROL (PLC)
							;0=PORTS A AND B OPERATE INDEPENDENTLY
							;1 = PORTS A AND B ARE LINKED PORT A ENABLE (PAE)
_PAE		equ		1<<2	;PORT A ENABLE (PAE)
_LC1		equ		1<<1	
_LC0		equ		1<<0	;COUNTER/TIMER LINK CONTROLS (LC)
							;LC1 LC0
							;0	0 COUNTER/TIMERS INDEPENDENT
							;0	1 C/T 1'S OUTPUT GATES C/T 2
							;1	0 C/T 1'S OUTPUT TRIGGERS C/T 2
							;1 	1 C/T 1'S OUTPUT IS C/T 2'S COUNT INPUT
;*****************************************************************************
;Port A/B Specification Registers
;Register Name
_PMSA		equ		$20		;Port A’s Mode Specification
_PMSB		equ		$28		;Port B’s Mode Specification
_PTS1		equ		1<<7	;PORT TYPE SELECTS (PTS1)
_PTS0		equ		1<<6	;PORT TYPE SELECTS (PTS0)

									;PTS1 PTS0
									;0 		0 BIT PORT
									;0		1 INPUT PORT
									;1		0 OUTPUT Font
									;1		1 BIDIRECTIONAL PORT
_ITB		equ		1<<5	;INTERRUPT ON TWO BYTES (ITB)
_SB			equ		1<<4	;SINGLE BUFFERED MODE (SB)
_IMO		equ		1<<3	;INTERRUPT ON MATCH ONLY (IMO)
_PMS1		equ		1<<2
_PMS0		equ		1<<1	;PATTERN MODE SPECIFICATION BITS (PMS)
							;PMS1 PMSO
							;0		0 DISABLE PATTERN MATCH
							;0		1 "AND" MODE
							;1		0 “OR” MODE
							;1		1 "OR-PRIORITY ENCODED VECTOR” MODE
_LPM		equ		1<<0	;LATCH ON PATTERN MATCH (LPM) (BIT MODE)
_DTE		equ		1<<0	;DESKEW TIMER ENABLE (DTE) (HANDSHAKE MODES)
;*******************************************************************
_DDPA		equ		$22		;Port A’s Data Path Polarity
_DDPB		equ		$2A		;Port B’s Data Path Polarity
_DDPC		equ		$05		;Port C’s Data Path Polarity
							;DATA PATH POLARITY (DPP) 0 = NON-lNVERTlNQ 1= INVERTING
_DDA		equ		$23		;Port A’s Data Direction
_DDB		equ		$2B		;Port B’s Data Direction
_DDC		equ		$06		;Port C’s Data Direction
							;DATA DIRECTION (DD) 0 = OUTPUT BIT 1 = INPUT BIT
;*****************************************************************************
_SIOA		equ		$24		;Port A’s Special I/O Control
_SIOB		equ		$2C		;Port B’s Special I/O Control
_SIOC		equ		$07		;Port C’s Special l/O Control 
							;SPECIAL INPUT/OUTPUT (SIO) 0 = NORMAL INPUT OR 
							;OUTPUT 1 = OUTPUT WITH OPEN DRAIN OR
							;INPUT WITH-1's CATCHER
;***************************************************************************
_PDRA		equ		$0D		;Port A’s Data (can be accessed directly)
_PDRB		equ		$0E		;Port B’s Data (can be accessed directly)
_PDRC		equ		$0F		;Port C’s Data (can be accessed directly)
									;0=Writing of corresponding LSB enabled
									;1=Writing of corresponding LSB inhibited
;*****************************************************************************
_PPA		equ		$25		;Port A’s Pattern Polarity
_PPB		equ		$2D		;Port B’s Pattern Polarity
_PTA		equ		$26		;Port A’s Pattern Transition
_PTB		equ		$2E		;Port B’s Pattern Transition
_PMA		equ		$27		;Port A’s Pattern Mask
_PMB		equ		$2F		;Port B’s Pattern Mask
							;PM		PT		PP		PATTERN SPECIFICATION
							;0 		0 		X		BIT MASKED OFF
							;0		1		X		ANY TRANSITION
							;1		0		0		ZERO
							;1		0		1		ONE
							;1		1		0		ONE TO ZERO TRANSITION 
							;1		1		1		ZERO-TO-ONE TRANSITION 
;*****************************************************************************
_IVRA		equ		$02		;Port A’s Interrupt Vector
_IVRB		equ		$03		;Port B’s Interrupt Vector
_IVRCT		equ		$04		;Counter/Timer’s Interrupt Vector
_CVR		equ		$1F		;Current Vector (READ ONLY)
							;INTERRUPT VECTOR BASED ON HIGHEST PRIORITY UNMASKED IP.
							;IF NO INTERRUPT PENDING ALL 1's OUTPUT.
;*****************************************************************************
;Most Often Accessed Registers
;Register Name
_PCSA		equ		$08		;Port A’s Command and Status
_PCSB		equ		$09		;Port B’s Command and Status
_CT1CS		equ		$0A		;Counter/Timer l’s Command and Status
_CT2CS		equ		$0B		;Counter/Timer 2’s Command and Status
_CT3CS		equ		$0C		;Counter/Timer 3’s Command and Status
_IUS		equ		1<<7	;INTERRUPT UNDER SERVICE (IUS)
_IE			equ		1<<6	;INTERRUPT ENABLE (IE)
_IP			equ		1<<5	;INTERRUPT PENDING (IP)
							;IUS, IE, AND IP ARE WRITTEN USING THE FOLLOWING CODE:
							;NULL CODE  		0	0	0
_CLEAR_IP_IUS	equ	%001<<5;CLEAR IP & IUS		0	0	1
_SET_IUS	equ		%010<<5	;SET IUS			0	1	0
_CLEAR_IUS	equ		%011<<5	;CLEAR IUS			0	1	1
_SET_IP		equ		%100<<5	;SET IP				1	0	0
_CLEAR_IP	equ		%101<<5	;CLEAR IP			1	0	1
_SET_IE		equ		%110<<5	;SET IE				1	1	0
_CLEAR_IE	equ		%111<<5	;CLEAR IE			1	1	1
_EAN		equ		1<<4	;INTERRUPT ERROR (EAN) (READ ONLY)
_ORE		equ		1<<3	;OUTPUT REGISTER (READ ONLY)
_IRF		equ		1<<2	;INPUT REGISTER FULL (READ ONLY)
_PMF		equ		1<<1	;PATTERN MATCH FLAG (PMF) (READ ONLY)
_IOE		equ		1<<0	;INTERRUPT ON ERROR (IOE)
	
;*****************************************************************************
;Timer Related Options
_EAR		equ		1<<4	;INTERRUPT ERROR (EAR) (READ ONLY)
_RCC		equ		1<<3	;READ COUNTER CONTROL (RCC) (READ/SET ONLY)
							; -CLEARED BY READING CCR LSB)
_GCB		equ		1<<2	;GATE COMMAND BIT (GCB)
_TCB		equ		1<<1	;TRIGGER COMMAND BIT (TCB) (WRITE ONLY - READ RETURNS 0)
_CIP		equ		1<<0	;COUNT IN PROGRESS (CIP) (READ ONLY)
;*****************************************************************************
;Counter/Timer Related Registers
_CTMS1		equ		$1C		;Counter/Timer l’s Mode Specification
_CTMS2		equ		$1D		;Counter/Timer 2’s Mode Specification
_CTMS3		equ		$1E		;Counter/Timer 3’s Mode Specification
_C_SC		equ		1<<7	;CONTINUOUS SINGLE CYCLE (C/SC=1)
_EOE		equ		1<<6	;EXTERNAL OUTPUT ENABLE (EOE) 	[Port B4/12,B0/8,C0/19]
_ECE		equ		1<<5	;EXTERNALCOUNT ENABLE (ECE) 	[Port B5/13,B1/9,C1/20]
_ETE		equ		1<<4	;EXTERNAL TRIGGER -ENABLE (ETE) [Port B6/14,B2/10,C2/21]
_EGE		equ		1<<3	;EXTERNAL GATE ENABLE (EGE)		[Port B7/15,B3/11,C3/22]
_REB		equ		1<<2	;RETRIGGER ENABLE BIT (REB)
_DCS1		equ		1<<1
_DCS0		equ		1<<0	;OUTPUT DUTY CYCLE SELECTS (DCS)
									;DCS1 DCS0
_PULSE 		EQU 0			;0 		0	PULSE OUTPUT
_ONESHOT	EQU	1			;0		1 	ONE-SHOT OUTPUT
_SQUAREWAVE	EQU	2			;1		0 	SQUARE-WAVE OUTPUT
									;1		1 	00 NOT SPECIFY

;*****************************************************************************
_CTCC1M		equ		$10		;Counter/Timer l’s Current Count-MSBs
_CTCC1L		equ		$11		;Counter/Timer l’s Current Count-LSBs
_CTCC2M		equ		$12		;Counter/Timer 2’s Current Count-MSBs
_CTCC2L		equ		$13		;Counter/Timer 2’s Current Count-LSBs
_CTCC3M		equ		$14		;Counter/Timer 3’s Current Count-MSBs
_CTCC3L		equ		$15		;Counter/Timer 3’s Current Count-LSBs
;*****************************************************************************
_CTTC1M		equ		$16		;Counter/Timer l’s Time Constant-MSBs
_CTTC1L		equ		$17		;Counter/Timer l’s Time Constant-LSBs
_CTTC2M		equ		$18		;Counter/Timer 2’s Time Constant.MSBs
_CTTC2L		equ		$19		;Counter/Timer 2’s Time Constant-LSBs
_CTTC3M		equ		$1A		;Counter/Timer 3’s Time Constant-MSBs
_CTTC3L		equ		$1B		;counter/Timer 3’s Time Constant-LSBs
;*****************************************************************************
_PHSA		equ		$21		;Port A’s Handshake Specification
_PHSB		equ		$29		;Port B’s Handshake Specification
_HTS1		equ		1<<7
_HTS0		equ		1<<6	;HANDSHAKE TYPE SPECIFICATION I I - BITS (HTS)
							;HTS1 HTS0
							;0		0 	INTERLOCKED HANDSHAKE
							;0		1 	STROBED HANDSHAKE
							;1		0 PULSED HANDSHAKE
							;1		1 THREE-WIRE HANDSHAKE
_RWS2		equ		1<<5	
_RWS1		equ		1<<4
_RWS0		equ		1<<3	;REQUEST/WAIT SPECIFICATION BITS -(RWS_									
							;RWS2	RWS1	RWSO	FUNCTION
							;0		0		0		REQUEST/WAIT DISABLED
							;0		0		1		OUTPUT WAIT
							;0		1		1		INPUT WAIT
							;1		0		0		SPECIAL REQUEST
							;1		0		1		OUTPUT REQUEST
							;1		1		1		INPUT REQUEST
_DTS3		equ		1<<2
_DTS2		equ		1<<1
_DTS1		equ		1<<0	;DESKEW TIME  SPECIFICATION BITS (DTS)
							;SPECIFIES THE MSB's OF DESKEW TIMER TIME CONSTANT. LSB IS FORCED 1
;********************************************************************
