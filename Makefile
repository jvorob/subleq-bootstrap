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

srcdir=.
objdir=build

sources := $(wildcard $(srcdir)/*.c)
headers := $(wildcard $(srcdir)/*.h)
objects := $(patsubst $(srcdir)/%.c,$(objdir)/%.o,$(sources))
bin_name := interp

.PHONY: all
all: $(bin_name)

$(bin_name): $(objects)
	$(CC) $(CFLAGS) $(objects) -o $@


$(objects): $(headers)

$(objdir)/%.o: $(srcdir)/%.c
	@[ -d $(objdir) ] || mkdir $(objdir)
	$(CC) $(CFLAGS) -c -o $(objdir)/$*.o $<



.PHONY: clean
clean:
	rm -f $(objdir)/*
	rm -f $(bin_name)
