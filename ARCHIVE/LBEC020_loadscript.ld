	/*
	*------------------------------------------------------------
	*                                  ___ ___ _
	*  ___ ___ ___ ___ ___       _____|  _| . | |_
	* |  _| . |_ -|  _| . |     |     | . | . | '_|
	* |_| |___|___|___|___|_____|_|_|_|___|___|_,_|
	*                     |_____|     firmware v1.2
	* ------------------------------------------------------------
	* Copyright (c)2020 Ross Bamford
	* See top-level LICENSE.md for licence information.
	*
	* Link script for code that is loaded by the serial-receive
	* boot loader.
	* ------------------------------------------------------------
	*/

	/*OUTPUT_FORMAT("binary")*/
	
	ENTRY(eeprom)

	slowram  = 0x0000100000;          /* start of ram support memory           */
	fastram  = 0x0000800000;          /* start of ram support memory           */
	eeprom = 0x00000000;

	MEMORY
	{
	SLOWRAM  : org = slowram,  l = 0x007FFFF     /* program can use all RAM > 4K */
	EEPROM  : org = eeprom,  l = 0x0007FFFF     /* program can use all RAM > 4K */
	}

	/* program configuration symbols that may be useful to override */
	/* using linker option: --defsym=<symbol_name>=<value>          */
	PROVIDE(_STACK_SIZE     = 0x00001000);  /* 16KB default stack size      */

	/*Periferal addresses */
	PROVIDE( OutPort        = PBDR);	      /*  Adress port B PI/T Outport */
	PROVIDE( IO_LS373       = $8FFE00);
	PROVIDE( Base68681      = $8FFE81);
	PROVIDE( Base68230      = $8FFF01);
	PROVIDE( Base6840 		= $8FFF81);
	PROVIDE( Base8536 		= $8FFF41);

	/* firmware absolute addresses (generally not useful to override)  */
	PROVIDE(_INITIAL_STACK  = 0x00000000);  /* firmware stack top (mem top) */
	PROVIDE(_WARM_BOOT      = 0x00000004);  /* firmware warm boot address   */

	PROVIDE(_MFP_VECTORS    = 0x00000100);  /* MFP interrupt vector base    */

	/* system data block */
	
	PROVIDE(_SDB_MAGIC      = 0x00000400);  /* SDB magic number             */
	PROVIDE(_SDB_STATUS     = 0x00000404);  /* SDB status code              */
	PROVIDE(_SDB_RSRVD_408  = 0x00000408);  /* SDB reserved                 */
	PROVIDE(_TIMER_100HZ    = 0x0000040C);  /* 100Hz timer counter          */
	PROVIDE(_EASY68K_ECHOON = 0x00000410);  /* Easy68k 'echo on' flag       */
	PROVIDE(_EASY68K_PROMPT = 0x00000411);  /* Easy68k 'prompt on' flag     */
	PROVIDE(_EASY68K_SHOWLF = 0x00000412);  /* Easy68k 'LF display' flag    */
	PROVIDE(_SDB_RSRVD_413  = 0x00000413);  /* SDB reserved                 */
	PROVIDE(_SDB_MEM_SIZE   = 0x00000414);  /* contiguous memory size       */
	
	/* NOTE: These need to be kept in sync with firmware equates.S! */
	
	PROVIDE(_EFP_TABLE      = 0x00000420);  /* Extension function ptr table */

	PROVIDE(_EFP_PRINT      = 0x00000420);  /* Print string to console      */
	PROVIDE(_EFP_PRINTLN    = 0x00000424);  /* Print string with CR+LF      */
	PROVIDE(_EFP_PRINTCHAR  = 0x00000428);  /* Print a character            */
	PROVIDE(_EFP_HALT       = 0x0000042C);  /* Disable interrupts and halt  */
	PROVIDE(_EFP_SENDCHAR   = 0x00000430);  /* Send a character via UART    */
	PROVIDE(_EFP_RECVCHAR   = 0x00000434);  /* Receive a character via UART */
	PROVIDE(_EFP_CLRSCR     = 0x00000438);  /* Clear screen (optional)      */
	PROVIDE(_EFP_MOVEXY     = 0x0000043C);  /* Set cursor position (opt.)   */
	PROVIDE(_EFP_SETCURSOR  = 0x00000440);  /* Enable/disable cursor (opt.) */
	PROVIDE(_EFP_RESRVD_444 = 0x00000444);  /* reserved                     */
	PROVIDE(_EFP_PROGLOADER = 0x00000448);  /* Initial program loader       */
	PROVIDE(_EFP_SD_INIT    = 0x0000044C);  /* SD Card init                 */
	PROVIDE(_EFP_SD_READ    = 0x00000450);  /* SD Card read                 */
	PROVIDE(_EFP_SD_WRITE   = 0x00000454);  /* SD Card write                */
	PROVIDE(_EFP_SD_REG     = 0x00000458);  /* SD Card register read        */
	
	/* ROM absolute addresses */
	PROVIDE(_FIRMWARE       = 0x00FC0000);  /* firmware address             */
	PROVIDE(_FIRMWARE_REV   = 0x00FC0400);  /* firmware revision code       */

	/* NOTE: rev1.1 used 0x00028000 (but init now position independent)     */
	PROVIDE(_LOAD_ADDRESS   = clearLCD);  /* firmware KERNEL_LOAD_ADDRESS */
	PROVIDE(_RUN_ADDRESS    = ORIGIN(RAM)); /* start of user memory         */
	
	PROVIDE(_Init_HC44780 	= 0x10A010); 
	PROVIDE(_string_subroutines = 0x109400);
	PROVIDE(_interrupt_vectors =  0x108400);
	PROVIDE(_MainProgram 	= 0x100000);
	PROVIDE(_LAB_Program 	= 0x110000);
	PROVIDE(_FlashTest 		= 0x120000);
	PROVIDE(_Kermittest 	= 0x130000);
	PROVIDE(_8536_Space 	= 0x140000);
	PROVIDE(_HEAP_SPACE 	= 0x150000);
	PROVIDE(_MonitorProgram = 0x160000);

	PROVIDE( scratchbuf	= 	0xFC000);
	PROVIDE( Cstr1		=	0xFC100);
	PROVIDE( Cstr2		=	0xFC200);
	PROVIDE( InText		=	0xFC300);
	PROVIDE( keyBufPtr  =	0xFC400);	
	PROVIDE( keyInputBuffer = 0xFC410);	
	PROVIDE( termBufPtr  =	0xFC500);	
	PROVIDE( termOutputBuffer = 0xFC510);	

	PROVIDE( inputqueue	=	0xFC600); 	
	PROVIDE( outputqueue	=	0xFC800);
	PROVIDE( suspend_for_input	=	0xFCA00);
	PROVIDE( suspend_for_output	=	0xFCA04);
	PROVIDE( xoff_typed		=	0xFCA08);
	PROVIDE( loop_sw		=	0xFCA0C);		
	PROVIDE( esc_seq_buf	=	0xFCA10);		/* buffer for escape sequence */
	PROVIDE( esc_seq_ptr	=	0xFCA20);		/* pointer for the buffer */
	PROVIDE( collect_esc_seq =	0xFCA24);		/* escape seq flag */
	PROVIDE( decbuf			=	0xFCA30);
	PROVIDE( dumpStack_LO	=	0xFCA60);
	PROVIDE( dumpStack_HI	=	0xFCA68);
	PROVIDE( counterActive	=	0xFCA80);		/* the 68681 counter interrupt active */
	PROVIDE( rec_trInt		= 	0xFCA84);			/* 	receive/transmit interrupts on  */
	PROVIDE( recOnlyInt 	=	0xFCA88);			/*	just receiver interrupts on  */
	PROVIDE( allIntOff		=	0xFCA8C);			/* no interrups receiv/transmit  */
	PROVIDE( PCval			=	0xFCA90);
	PROVIDE( SREC_GoAdr		= 	0xFCA98); 			/* startaddress from first S1/S2/S3 record */
	PROVIDE( SREC_Buf		=	0xFCB00);			/* monitor PCvalue register */



	SECTIONS
	{


		.assemblycode _MainProgram :
			{
			_init = .;
			KEEP( *.elf(.assemblycode))
			_init_end = .;
			} > SLOWRAM


		LAB_Program _LAB_Program  : 
			{

			KEEP( *.elf(LAB_Program))

			} > SLOWRAM

		SST39WR _FlashTest  : 
			{

			KEEP( *.elf(SST39WR))

			} > SLOWRAM

		C_Kermit_test  _Kermittest  : 
			{
			KEEP( *.elf(C_Kermit_test))
			} > SLOWRAM

		C_8536Init  _8536_Space  : 
			{
			KEEP( *.elf(C_8536Init))
			} > SLOWRAM


		initvectors eeprom :
			{
			KEEP( *.elf(initvectors))
			} > EEPROM

		.monitor _MonitorProgram :
			{
			_monitor = .;
			KEEP( *.elf(.monitor))
			_monitor_end = .;
			} > SLOWRAM


		strings_HEAP  ALIGN(0x40) :
			{
			
			KEEP( *.elf(strings_HEAP))
			
			} > SLOWRAM



		.sec_HD44780_Init ALIGN(0x40) :

			{
				KEEP( *.elf(.sec_HD44780_Init))

				main_init_end = .;
			} >SLOWRAM
		
		/*int_vec_subroutines ALIGN (0x40) : */
		int_vec_subroutines  _interrupt_vectors  : 
			{
			int_vec_routines = .;	
			KEEP( *.elf(int_vec_subroutines))
			int_vec_routines_end = .;	
			} > SLOWRAM


		string_subroutines ALIGN(0x40) :
			{	
				KEEP( *.elf(string_subroutines))

				string_routines_end = .;	
			} > SLOWRAM




		heap_space _HEAP_SPACE :
			{
			_heap1 = .;	
			KEEP(	*.elf(heap_space1))
			_heap1_end = .;	
			}> SLOWRAM

		
		



	/*
		. = 0xA00010;
		string_routines ALIGN(0x40): { *.elf(string_subroutines) }

		. = . +0x20;
		lcd_routines  ALIGN(0x40): { *.elf(HD44780_init) }
	*/

	/*
	.text.init :
	{
		_init = .;
		KEEP(*(.init))      /* KEEP() "anchors" section for gc-sections /
		_init_end = .;
	} > RAM

	.text.postinit :
	{
		_postinit = .;
		KEEP(*(.postinit))  /* KEEP() "anchors" section for gc-sections /
		. = ALIGN(4);       /* long align for init.S copying /
		_postinit_end = .;
	} > RAM

	.text ALIGN(4) :
	{
		_code = .;
		*(.text*)
		*(.rodata*)
		_code_end = .;
	} > RAM

	.data ALIGN(4) :
	{
		_data_start = .;
		*(.data*)
		. = ALIGN(4);       /* long align for init.S copying /
		_data_end = .;
	} > RAM

	.bss ALIGN(4) :
	{
		_bss_start = .;
		*(.bss*)
		*(COMMON)
		. = ALIGN(4);       /* long align for kinit clearing /
		_bss_end = .;
	} > RAM
	*/
	}

	_end = .;
