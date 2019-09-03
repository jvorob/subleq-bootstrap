Writing an emulator and tooling for the SUBLEQ architecture, a one-instruction computing arch

Goal is to write a series of bootstrapping assemblers in subleq, up to a useful macro language

# Compilation

Running `make clean && make` should build the c components

# Execution:

To build the base assembler, hex1.bin, run:

`./hex1 <asm/hex1.hex1 >hex1.bin`


The various assemblers can be used to compile source files like so:

`./sleqrun hex1.bin <asm/hello.hex1 >hello.bin`


To run a binary, do

`./sleqrun hello.bin`


Program gets in/output on stdin/stdout, sleqrun emulator prints debugging info on stderr, 
For full program trace, uncomment the "#define DEBUG\_LOGGING" in src/sleqrun\_main.c

# Bootstrapping:

The goal of this project is that later assemblers are built by earlier ones
e.g. : hex2 is a more powerful langauge than hex1, but harder to assemble

To generate hex2.bin, a hex2 assembler, do the following steps:

            hex1         <asm/hex1.hex1    >hex1.bin
    sleqrun hex1.bin     <asm/hex2_bs.hex1 >hex2_bs.bin
    sleqrun hex2_bs.bin  <asm/hex2.hex2    >hex2.bin

Note, hex2\_bs.hex1, is bigger, slower, and buggier, thant hex2.hex2.
Both can compile hex2 code, but I could write hex2.hex2 more cleanly because I could use the
new features of the hex2 language.


# ASSEMBLER VERSIONS

### hex1: 
- handles [0-1A-Fa-f], whitespace, and '#' line comments
- reads from stdin, writes to stdout
- outputs all things as dwords, little-endian


### hex2: 
- backwards compatible with hex1
- Supports ?, outputting address of next word
- Supports @, seeking ahead to specified address (e.g. @1FF)
- @ errors out if seeking backwards, preventing bugs
- Different exit codes: 0 for succ, 1 for bad char, 2 for seek backwards


### hex3:
- Supports ?+offset, which should make it easy to code indirect access and jumps


### hex4: (TODO)
- Supports '-NUM', e.g. -1, -FF (nice to have and shouldn't be too hard at this point)
- Support ?-offset
- +/- literals (for now only support 0-F, 00-F0, 000-F00, and 0000-F000, i.e. one nibble set)



# Misc. Thoughts

A fun test that I used while writing hex2.hex2 was the following command:

    sleqrun hex2_bs.bin <asm/hex2.hex2  >hex2.bin1 && 
    sleqrun hex2.bin1   <asm/hex2.hex2  >hex2.bin2 &&
    sleqrun hex2.bin2   <asm/hello.hex2 >hello.bin &&
    sleqrun hello.bin &&
    diff hex2.bin1 hex2.bin2

This first uses the old assembler, hex2\_bs.bin, to build hex2.hex2. This sees if the new code will compile at all.
The next step uses the newly-assembled hex2.bin1 to build its own source into hex2.bin2
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
