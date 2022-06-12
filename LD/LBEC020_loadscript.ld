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

	/* system data block */
	/* NOTE: These need to be kept in sync with firmware equates.S! */
	
	
	
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

	SECTIONS
	{


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


/*
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
/*		
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
*/
		
		



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
