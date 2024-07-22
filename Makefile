MAKEFLAGS += --warn-undefined-variables



# C/C++ compiler
CC = gcc  

# Compiler and linker flags (warning levels, optimisation level, 
# include debugging symbols, add include search path, add library search path)
CFLAGS = -Wall -std=gnu11
LDFLAGS = -L./src/libs

# Object files
#OBJS = obj1.o obj2.o obj3.o
# Executable name

srcdir=src
objdir=build
testdir=test

sources := $(wildcard $(srcdir)/*.c)
headers := $(wildcard $(srcdir)/*.h)
objects := $(patsubst $(srcdir)/%.c,$(objdir)/%.o,$(sources))
main_objs :=   $(filter     %main.o, $(objects))
shared_objs := $(filter-out %main.o, $(objects))
bins    := $(patsubst $(objdir)/%_main.o, %, $(main_objs))

.PHONY: all
all: $(bins)

.PHONY: debug_make
debug_make:
	@echo $(shared_objs)
	@echo $(bins)



$(bins): %:$(objdir)/%_main.o $(shared_objs)
	$(CC) $(CFLAGS) $^ -o $@


$(objects): $(headers)

$(objdir)/%.o: $(srcdir)/%.c
	@[ -d $(objdir) ] || mkdir $(objdir)
	$(CC) $(CFLAGS) -c -o $(objdir)/$*.o $<




.PHONY: tower
tower: asm1.bin

asm1.bin: asm/asm1.asm1  asm1_bs.bin
	./sleqrun asm1_bs.bin <$< >$@

asm1_bs.bin: asm/asm1.hex3  hex3.bin
	./sleqrun hex3.bin <$< >$@

hex3.bin: asm/hex3.hex3  hex3_bs.bin
	./sleqrun hex3_bs.bin <$< >$@

hex3_bs.bin: asm/hex3.hex2 hex2.bin
	./sleqrun hex2.bin <$< >$@

hex2.bin: asm/hex2.hex2 hex2_bs.bin
	./sleqrun hex2_bs.bin <$< >$@

hex2_bs.bin: asm/hex2.hex1 hex1.bin sleqrun
	./sleqrun hex1.bin <$< >$@

hex1.bin: asm/hex1.hex1 hex1
	./hex1 <$< >$@



# ========== TESTS
.PHONY: test
test: test_asm1_bin

.PHONY: test_asm1_bin
test_asm1_bin: asm1.bin sleqrun
	@./sleqrun asm1.bin <asm/hello.asm1 >$(testdir)/hello_asm1_bin.out 2>/dev/null
	@diff -q $(testdir)/hello_asm1_bin.out $(testdir)/hello_asm1_bin.expected || \
		echo "Test $@ failed"	

.PHONY: test_too_many_labels 
test_too_many_labels: $(testdir)/test_too_many_labels.asm1 asm1.bin
	@./sleqrun asm1.bin <$< 2>/dev/null >/dev/null ; exit_code="$$?"; \
	if [ $$exit_code -ne 5 ]; then \
		echo "Test $@ failed, expected to halt with code 5"; \
	fi 


.PHONY: clean
clean:
	rm -f $(objdir)/*
	rm -f $(bins)
	rm -f *.bin
	rm -f $(testdir)/*.out
