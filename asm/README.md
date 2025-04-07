# Assembler Bootstrapping

While we began with a (hex)-assembler written in C, the goal is to bootstrap these assemblers into subleq so that they can be self-hosting.

The first steps is to replace our initial hex1 assembler (written in C), with a subleq
program that does the same thing.

`./hex1 <asm/hex1.hex1 >hex1.bin`

Now we can assemble subleq binaries without using any C programs (other than the emulator itself). This also makes the hex1 assembler self-hosting.
    $ ./sleqrun hex1.bin  <asm/hello.hex1 >hello.bin
        Loading binary from hex1.bin... 656 words
        ======= Running 16bit, 2MHz:
        ======= Halted with code 0 after 9916 steps
    $ ./sleqrun hello.bin
        Loading binary from hello.bin... 80 words
        ======= Running 16bit, 2MHz:
        Hello World
        ======= Halted with code 0 after 103 steps


# Bootstrapping the assembler.

The goal of this project is that later assemblers are built by earlier ones
e.g. : hex2 is a more powerful langauge than hex1, but harder to assemble

To generate hex2.bin, a hex2 assembler, do the following steps:

            hex1         <asm/hex1.hex1    >hex1.bin
    sleqrun hex1.bin     <asm/hex2_bs.hex1 >hex2_bs.bin
    sleqrun hex2_bs.bin  <asm/hex2.hex2    >hex2.bin

Note, hex2\_bs.hex1, is bigger, slower, and buggier, thant hex2.hex2.
Both can compile hex2 code, but I can write hex2.hex2 more cleanly using the features it adds (which allow me to write position-independent code). The final `hex2.bin` is able to compile my hello world program in 8302 cycles, while the initial `hex2_bs.bin` does the same in 11036.

The subsequent steps in the bootstrapping process follow this general procedure,
first cobbling together the new language features in the weaker language, before then rewriting itself to take advantage of the new language features we just implemented to optimize/refactor. I continue this process until


# ASSEMBLER VERSIONS

### hex1:
- handles [0-1A-Fa-f], whitespace, and '#' line comments
- reads from stdin, writes to stdout
- outputs all things as dwords, little-endian

Incredibly limited, spacing between code chunks needs to be manually created by inserting zeroes as padding, and moving code around is almost impossible.
Code looks like this:

    # keep looping until newline, then go to next_char
    #parse_comment:
      20 20 73    #70:  X X  (clear X)
      28 28 76    #73:  C C
       8 28 79    #76:  I C  (read -getc() into C)
      28 20 7C    #79:  C X  (flip; X holds +getc())
     109 20 70    #7C:  $9 X comment    ; if X  <0xA
       4 20 40    #7F:  $1 X next_char  ; if X == 0xA
      20 20 73    #82:  X X comment+3   ; else (>0xA),
       0  0 88    #85:
       0  0 8B    #88:
       0  0 8E    #8B:  padding bytes (can't move code blocks around)

### hex2:
- backwards compatible with hex1
- Supports ?, outputting address of next word
- Supports @, seeking ahead to specified address (e.g. @1FF)
- @ errors out if seeking backwards, preventing bugs
- Different exit codes: 0 for success, 1 for bad char, 2 for seek backwards

With these additions, reorganizing code becomes much easier, as I can leave space in memory by just specifying to seek ahead to an address with e.g. `@480`. The ability to specify the next address with `?` is also crucial: unless I want to branch, every instruction needs to specify the address of the following instruction, and without the `?` feature that means that moving a code block also requires rewriting every one of those addresses. These features together make it feasible to start adding some more sophisticated parsing:

### hex3:
- Supports ?+offset, which should make it easy to code indirect access and jumps

In subleq, the only way to do indirect memory accesses (i.e. reading a pointer) is with self-modifying code: you write a variable value into the operand of the next instruction, then execute that instruction to load from the address, like so:

```
    # FETCH_SUBTRACT: X -= *(-P)
    ?+5 ?+4   ?  #   : Q   Q     # clear operand Q (2 insts ahead)
    [P] ?+1   ?  #   : P   Q     # subtract address at P from operand Q
    _ [X]   ?  #   : Q:_ X     # subtract value at addr Q from X
```

Being able to specify-relative offsets like this lets us write snippets like this that are position independent (and so can be copy/pasted), which is crucial for implementing the string tables I will need for the next step.

### asm1:
- adds labels, (single-pass, so backwards labels only)
- made like "label:", referenced like "label"
- labels must start with [A-Za-z_], continue with [A-Za-z0-9\_]
- numeric constants must now start with a digit (e.g. 0A)

At this point we finally have the ability to make arbitrary string labels and no longer need to remember magic constants, which gives access to named variables, function names, and finally gets us close to something resembling a language, like so:

    # Y will hold len so far
    # V(-) will hold ptr to store char
    read_label:
      Y Y;                  # clear len
      V V; tbl_end V; 4 V;  # V := -(tbl_end + 4)
                            # leaves 1 word for len field

    read_label_loop:        # (Tests if C is a valid label char)
      X X; C_ X;            # X := +(next char)
                  # switch(X) {
      i20  X ;    #   X -= 0x20
      iF   X end  #   X -= 0x0F,  if <=2F, jmp end
      iA   X cont #   X -= 0x0A,  if <=39, ('0'-'9'), jmp cont.
      i7   X end  #   X -= 0x07,  if <=40
      ...

### asm2:
- added option for literal strings, useful for forth (e.g. "hello world")

### TODO: Future assembler improvements ??
- Supports '-NUM', e.g. -1, -FF (nice to have and shouldn't be too hard at this point)
- Support ?-offset
- Rewrite the assemblers to not use the mem-mapped ALU?
  (would need to implement shift-up and extract-bytes as lookup-tables?)

### Misc. TODO
- Add proper arg handling to sleqrun (--debug flag)
- Sleqrun let user specify args to guest program on command line (or from file?)
- Sleqrun: optional 2nd and 3rd options for stdin file and stdout file

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
