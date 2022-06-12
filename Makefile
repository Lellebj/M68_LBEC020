
# My  LICENSE ???


SYSINCDIR?=Include
SYSLIBDIR?=Lib
BUILD_DIR?=Build
LIBS=
DEFINES=-DM68K-EC020

# INIT=HC373_HC44780_MAIN_INIT
# IOLIB=INT_IO_Lib
# STR=string_convert_subroutines
# VEC=int_vector_subroutines
# MAIN=HC373_HD44780
# QUEUES=Queues
# TML=TimerIO_lib


#****************************************************
SOURCES    := $(wildcard *.asm)
OBJECTS    := $(patsubst %.asm,$(BUILD_DIR)/%.o,$(SOURCES))
# SOURCES = $(shell find  -maxdepth 1 -name "*$(S_EX)")
# OBJECTS = $(SOURCES:%$(S_EX)=$(BUILD_DIR)/%.o)


#****************************************************
$(info    SRC is $(SOURCES))
$(info    OBJ is $(OBJECTS))


S_EX=.asm
OBJ_EX=o

.SECONDEXPANSION:


# OBJECTS=$(INIT).o  $(IOLIB).o $(STR).o $(VEC).o $(MAIN).o $(QUEUES).o $(TML).o
#OBJECTS=$(INIT).o  $(IOLIB).o $(STR).o $(VEC).o $(MAIN).o $(QUEUES).o $(TML).o $(SRCONV).o

LDFLAGS=-T LD/LBEC020_loadscript.ld 	-L $(SYSLIBDIR)   -Map=$(MAP)  --oformat=elf32-m68k
ASFLAGS=-Felf -m68020 -m68882 -o $@ $(F_LIST) $(DEFINES)
CC=m68k-elf-gcc
LD=m68k-elf-ld
AS=vasmm68k_mot

#AS=m68k-elf-as
RM=rm -f

OBJCFLAGS=-I elf32-big -O srec --srec-len 200
# --srec-forceS3
_EXT=srec

#OBJCFLAGS=-I elf32-big -O verilog
#_EXT=bin
# symbolsrec verilog  binary ihex

# Output config
BINARY_BASENAME=68CE020
BINARY=$(BUILD_DIR)/$(BINARY_BASENAME).$(_EXT)



default: $(BINARY)



MAP=$@.map
LST=$@.lst
#F_LIST?=
F_LIST = -L $(LST)


DATE := $(shell date +"%Y-%m-%d_%H:%M")
# GIT_VERSION := $(shell git describe --long --dirty; git show -s --format='%ci')
# cat $< | sed -e "s/@@DATE@@/$(DATE)/g" -e "s/@@GIT_VERSION@@/$(GIT_VERSION)/g" | z80asm - -o $@ --list=$(basename $@).lst --label=$(basename $@).sym $(ASM_FLAGS)

# test_lev.o : Z80TT*.asm
# 		sed -ri "s/[12][0-9]{3}[-][01][0-9][-][0-3][0-9][_][0-6]{2}[:][0-6]{2}/$(DATE)/g" $^
# 		$(AS)  $(ASFLAGS)  $^


$(OBJECTS) : $$(patsubst $(BUILD_DIR)/%.o, %$(S_EX),$$@)
		mkdir -p $(@D)
		sed -ri "s/[2][0-9]{3}[-][0][0-9][-][0-3][0-9][_][0-9]{2}[:][0-9]{2}/$(DATE)/g" $^
		$(AS)  $(ASFLAGS)  $<
#		mv 	*.lis *.map *.o -t $(BUILD_DIR)
		echo


# %.o : %.asm
# 	$(AS) $(ASFLAGS) -o $@ $<

$(BINARY) : $(OBJECTS)
	$(LD) $(LDFLAGS) $^ -o $@ $(LIBS)
	chmod a-x $@

# mainChunk : $(BINARY)
# 	objcopy $(OBJCFLAGS) -j .assemblycode -j heap_space $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)
mainChunk : $(BINARY)
	objcopy $(OBJCFLAGS) -j .monitor  $(BINARY) $(BUILD_DIR)/$@.$(_EXT)

# vecChunk : $(BINARY)
# 	objcopy $(OBJCFLAGS) -j int_vec_subroutines $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)

# stringChunk : $(BINARY)
# 	objcopy $(OBJCFLAGS) -j string_subroutines $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)

# initChunk : $(BINARY)
# 	objcopy $(OBJCFLAGS) -j .sec_HD44780_Init  -j initvectors -j .monitor $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)
initChunk : $(BINARY)
	objcopy $(OBJCFLAGS)  -j initvectors  $(BINARY) $(BUILD_DIR)/$@.$(_EXT)

# LABProgram : $(BINARY)
# 	objcopy $(OBJCFLAGS) -j SST39WR -j LAB_Program -j C_8536Init $(BINARY) $(BUILDLIBDIR)/$@.$(_EXT)


.PHONY: all clean dump

# all: mainChunk  vecChunk stringChunk initChunk LABProgram
all: mainChunk  initChunk 



.PHONY: all clean load

clean: 
	$(RM) $(OBJECTS)  $(BUILD_DIR)/*.* libs/build/*.$(_EXT) *.lst *.o  *.def