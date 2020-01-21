MAKEFLAGS += --warn-undefined-variables

export PATH := .:$(PATH)  # otherwise some tests fail on the compas machine


# C/C++ compiler
CC = gcc  

# Compiler and linker flags (warning levels, optimisation level, 
# include debugging symbols, add include search path, add library search path)
CFLAGS = -Wall -std=c99 -Wno-unknown-pragmas
LDFLAGS = -L./src/libs

# Object files
#OBJS = obj1.o obj2.o obj3.o
# Executable name

srcdir=src
objdir=build

sources := $(wildcard $(srcdir)/*.c)
headers := $(wildcard $(srcdir)/*.h)
objects := $(patsubst $(srcdir)/%.c,$(objdir)/%.o,$(sources))
main_objs :=   $(filter     %main.o, $(objects))
shared_objs := $(filter-out %main.o, $(objects))
bins    := $(patsubst $(objdir)/%_main.o, %, $(main_objs))

.PHONY: all
all: $(bins)

.PHONY: test
test:
	@echo $(shared_objs)
	@echo $(bins)



$(bins): %:$(objdir)/%_main.o $(shared_objs)
	$(CC) $(CFLAGS) $^ -o $@


$(objects): $(headers)

$(objdir)/%.o: $(srcdir)/%.c
	@[ -d $(objdir) ] || mkdir $(objdir)
	$(CC) $(CFLAGS) -c -o $(objdir)/$*.o $<




.PHONY: tower
tower: hex3.bin

hex3.bin: asm/hex3.hex3  hex3_bs.bin
	sleqrun hex3_bs.bin <$< >$@

hex3_bs.bin: asm/hex3.hex2 hex2.bin
	sleqrun hex2.bin <$< >$@

hex2.bin: asm/hex2.hex2 hex2_bs.bin
	sleqrun hex2_bs.bin <$< >$@

hex2_bs.bin: asm/hex2.hex1 hex1.bin sleqrun
	sleqrun hex1.bin <$< >$@

hex1.bin: asm/hex1.hex1 hex1
	hex1 <$< >$@


.PHONY: clean
clean:
	rm -f $(objdir)/*
	rm -f $(bins)
	rm -f *.bin


# =====
# When synthesising this onto an fpga, we need to include the subleq binary 
# in with the C code, so it actually gets onto the chip
#
# To do this, we generate 'build/hardcoded_binary.h'

hls_subleq_src := asm/hello.hex3 # Which subleq file to preload the HLS core with
hls_subleq_cc  := hex3.bin       # which tool is it built with

$(objdir)/hardcoded_binary.h : $(objdir)/hls_subleq_prog.bin
	tools/bin_to_header.py $(objdir)/hls_subleq_prog.bin >$(objdir)/hardcoded_binary.h

$(objdir)/hls_subleq_prog.bin: $(hls_subleq_src) $(hls_subleq_cc)
	sleqrun $(hls_subleq_cc) <$(hls_subleq_src) >$(objdir)/hls_subleq_prog.bin


.PHONY: hls
hls: $(objdir)/hardcoded_binary.h $(srcdir)/sleqrun_main.c
	vivado_hls -i -f hls-script.tcl
