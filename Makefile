
# My  LICENSE ???


SYSINCDIR?=include
SYSLIBDIR?=libs/build/lib
BUILDLIBDIR?=build
LIBS=
DEFINES=-DM68K-HD44780

INIT=HC373_HC44780_MAIN_INIT
IOLIB=INT_IO_Lib
STR=string_convert_subroutines
VEC=int_vector_subroutines
MAIN=HC373_HD44780
QUEUES=Queues
TML=TimerIO_lib
#SRCONV=srec_convert

S_EX=.S
OBJ_EX=o
OBJECTS=$(INIT).o  $(IOLIB).o $(STR).o $(VEC).o $(MAIN).o $(QUEUES).o $(TML).o
#OBJECTS=$(INIT).o  $(IOLIB).o $(STR).o $(VEC).o $(MAIN).o $(QUEUES).o $(TML).o $(SRCONV).o

LDFLAGS=-T LD/HD44780_HC373.ld 	-L $(SYSLIBDIR)   -Map=$(MAP)  --oformat=elf32-m68k
ASFLAGS=-Felf -m68020 -m68882 $(F_LIST) $(DEFINES)
CC=m68k-elf-gcc
LD=m68k-elf-ld
AS=vasmm68k_mot
#AS=m68k-elf-as
RM=rm -f
OBJCFLAGS=-I elf32-big -O srec --srec-len 20
# --srec-forceS3
_EXT=srec
#OBJCFLAGS=-I elf32-big -O verilog
#_EXT=bin
# symbolsrec verilog  binary ihex

# Output config
BINARY_BASENAME=68CE020
BINARY=$(BUILDLIBDIR)/$(BINARY_BASENAME).$(_EXT)
MAP=$@.map
LST=$@.lst
#F_LIST?=
F_LIST = -L $(LST)


%.o : %.asm
	$(AS) $(ASFLAGS) -o $@ $<

$(BINARY) : $(OBJECTS)
	$(LD) $(LDFLAGS) $^ -o $@ $(LIBS)
	chmod a-x $@

mainChunk : $(BINARY)
	objcopy $(OBJCFLAGS) -j .assemblycode -j heap_space $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)

vecChunk : $(BINARY)
	objcopy $(OBJCFLAGS) -j int_vec_subroutines $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)

stringChunk : $(BINARY)
	objcopy $(OBJCFLAGS) -j string_subroutines $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)

initChunk : $(BINARY)
	objcopy $(OBJCFLAGS) -j .sec_HD44780_Init  -j initvectors -j .monitor $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)

LABProgram : $(BINARY)
	objcopy $(OBJCFLAGS) -j SST39WR -j LAB_Program -j C_8536Init $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)


.PHONY: all clean dump

all: mainChunk  vecChunk stringChunk initChunk LABProgram



.PHONY: all clean load

clean: 
	$(RM) $(OBJECTS)  $(BINARY) libs/build/*.$(_EXT) *.map *.lst
