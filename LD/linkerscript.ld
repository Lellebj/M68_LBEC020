
MEMORY
       {
         sram (rwx) : ORIGIN = 0, LENGTH = 512K
         flash (rx) : org = 0x500000, l = 1M
       }


SECTIONS
     {


       . = 0x1000;
       progs_: { *.elf(assemblycode) }

/*       . = 0x1400;
       adr_table: { *.elf(addresstable) }
*/



       . =  0x2000;
       int_vec_routines ALIGN(0x40): { *.elf(int_vec_subroutines) }

       . = 0x3000;
       string_routines ALIGN(0x40): { *.elf(string_subroutines) }

       . = 0x4000;
       lcd_routines  ALIGN(0x40): { *.elf(HD44780_init) }


       . = 0x0FC000;
       heap1:  {*.elf(heap_space1)}

       . = 0x0FC180;
       heap2:  {*.elf(heap_space2)}

       . = 0x500000; 
       init : { *.elf(initvectors) }
/*
       . = 0x500010;
       string_routines ALIGN(0x40): { *.elf(string_subroutines) }

       . = . +0x20;
       lcd_routines  ALIGN(0x40): { *.elf(HD44780_init) }
*/


     /* 
      .bss : { *.elf(.bss) }
      */
     }
