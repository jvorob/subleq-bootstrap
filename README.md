Writing an emulator and tooling for the SUBLEQ architecture (https://esolangs.org/wiki/Subleq), a one-instruction computing arch

Goal is to write a series of bootstrapping assemblers in subleq, up to a useful macro language

# Emulator

The emulator that all this code runs on is `src/sleqrun_main.c`, running `make clean && make` should build the C executable `sleqrun`.
This emulates a CPU whose one opcode is Subtract-and-Jump-if-Less-or-Equal-to-Zero. The word size is 16 bits, and several memory locations
are "magic", used to allow character based I/O, as well as implementing a memory-mapped ALU for a few bitwise operations 
(details of instruction on esolang wiki, details of emulator commented at top of `sleqrun_main.c`).

### SUBLEQ Execution:

Subleq binaries can be run with `./sleqrun foo.bin`,

Program gets in/output on stdin/stdout, sleqrun emulator prints debugging info on stderr, 
For full program trace, uncomment the "#define DEBUG\_LOGGING" in src/sleqrun\_main.c

# Assembler bootstrapping:

To create subleq binaries, I wrote a series of assemblers, the first of which, _hex1_, just handles ASCII hex values into a binary file of 16-bit words, and can support line comments.

The first implementeation of the _hex1_ "assembler" is written in C, called `./hex1`

I also hand-wrote an implementation of _hex1_ in SUBLEQ machine code, in `asm/hex1.hex1` 
We can use the C implementation of the _hex1_ assembler to make a binary from our SUBLEQ implementation.

`./hex1 <asm/hex1.hex1 >hex1.bin`

Now `hex1.bin` can run on the SUBLEQ machine to turn _hex1_ source into binaries, so it's officially self-hosting. 

### Hello World

We can use `hex1.bin` to compile and run a hello world program

    ./sleqrun hex1.bin <asm/hello.hex1 >hello.bin  # Use hex1.bin to compile hello.hex1 into a binary
    ./sleqrun hello.bin                            # Run the hello binary 

Which should print the expected output.

### Bootstrapping:

The goal of this project is that later assemblers are built by earlier ones.
The _hex1_ "language" is just raw machine code with comments, and is a pain to program in. 
_hex2_ and _hex3_ support additional syntax and address arithmetic that make it much
easier to rearrange and reuse code snippets.
Then _asm1_, can handle all of those as well as labels.

To generate asm1.bin, an asm1 assembler, I do the following steps:

            hex1         <asm/hex1.hex1    >hex1.bin      # initial C bootstraping to get hex1.bin
    sleqrun hex1.bin     <asm/hex2.hex1    >hex2_bs.bin   # use hex1 to build a dirty "bootstrap" implementation of hex2
    sleqrun hex2_bs.bin  <asm/hex2.hex2    >hex2.bin      # improved hex2 assembler written more cleanly in itself (hex2)
    ...                                                   # etc.
    sleqrun hex3.bin     <asm/asm1.hex3    >asm1.bin 

Running `make tower` will make all of these in sequence

Note, hex2\_bs, is bigger, slower, and buggier than hex2.
Both can compile _hex2_ code, but the implementation in `hex2.hex2` could be written more easily and cleanly using the
new features that it itself implemented, and compiled with the uglier `hex2\_bs.bin`.

# ASSEMBLER VERSIONS

### hex1: 
- handles [0-1A-Fa-f], whitespace, and '#' line comments
- reads from stdin, writes to stdout
- outputs all things as dwords, little-endian


### hex2: 
- Supports hex1
- Also supports ?, outputting address of next word (no more manually-threading together every instruction to its successor)
- Also supports @, seeking ahead to specified address (e.g. @1FF, to put jump targets at known addresses)
- @ errors out if seeking backwards, preventing bugs
- Different exit codes: 0 for succ, 1 for bad char, 2 for seek backwards


### hex3:
- Supports ?+offset, which makes it possible to express PC-relative memory accesses
- This means that expression that require self-modifying code (like indirect access and jumps) can be copy-pastable, making them feasible to use more broadly.


### asm1:
- Supports labels: (only going backwards though, it's single-pass)
- made like "label:", referenced like "label"
- labels must start with [G-Zg-z] or \_
- labels can contain [A-Za-z0-9\_]
- Also has a full string table as a result, so that might be useful for stuff?

### asm2: (PLAN)
- Allow for referencing labels later in the file
- (Read file into RAM to allow for 2-passes to achieve this)

### Misc Improvements for later? 
- Support '-NUM', e.g. -1, -FF (nice to have and shouldn't be too hard at this point)
- Support ?-offset
- +/- literals (for now only support 0-F, 00-F0, 000-F00, and 0000-F000, i.e. one nibble set)
- String constants ("Hello World")
- Rewrite the assemblers to not use the memory-mapped ALU 
  (would need to implement shift-up and extract-bytes using just SUBLEQ)
- Sleqrun: optional 2nd and 3rd options for stdin file and stdout file
- Add some form of debugger to sleqrun (maybe special exit code for debugger + inspector) [DONE]


# Miscellaneous thoughts/ramblings on bootstrapping

A fun test that I used while writing hex2.hex2 was the following command:

    sleqrun hex2_bs.bin <asm/hex2.hex2  >hex2.bin1 && 
    sleqrun hex2.bin1   <asm/hex2.hex2  >hex2.bin2 &&
    sleqrun hex2.bin2   <asm/hello.hex2 >hello.bin &&
    sleqrun hello.bin &&
    diff hex2.bin1 hex2.bin2

This first uses the old assembler, hex2\_bs.bin, to build hex2.hex2 into a first-generation binary, hex2.bin1. This sees if the new code will compile at all.
The next step uses the newly-assembled hex2.bin1 to build its own source into a second-generation binary hex2.bin2
This shows us if bin1 can actually run without crashing.
The third step uses bin2 to compile hello.hex2, to see if the output of bin1 is correct enough to not crash (which still doesn't mean bin1 is correct).
The next step runs the hello world program, to see if bin2 compiled hello.bin correctly.
Finally, we check if the two binaries are identical.


If the final program successfully prints "Hello World", we can infer a few things.
Since bin2 made a simple program that works, it's a mostly-working assembler, but we haven't tested it on complex programs. 
At the same time, bin2 is a complex program successfully assembled by bin1, 
so bin1 has been tested rigorously and works. 
If bin1 and bin2 are identical
and since we're pretty sure bin1 is correct, then that means that bin2 must also be correct.


This could be flawed reasoning if there are very obscure bugs that I haven't triggered, but in that case they don't affect me and there's not much I can do about them.

A more interesting way that hex2 could be broken is if there are "circular bugs", a bug in hex2.hex2 which affects complex programs, but in the case of hex2.hex2, only affects it enough to cause a similar minor bug in the binary. If it's a little too harmful, then bin2 would be more broken than bin1, and if you used bin2 to make bin3, that would be even more broken, until they stopped compiling entirely. 

However, it's possible to have a 'stable' bug, which, when the assembler is used to build its own source, made a binary that was broken in just the right way to cause the exact same bug in its "offspring". Alternatively, you could have something like a sequence, where bin1 has bug A, which causes bin2 to have bug B, which causes bin3 to have bug A again. Something like that.
