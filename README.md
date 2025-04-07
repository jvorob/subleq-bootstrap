Writing an emulator and tooling for a [SUBLEQ](https://esolangs.org/wiki/Subleq) instruction set, computer.

Goal is to write a series of bootstrapping assemblers in subleq, up to a useful macro language


# Compilation / execution

`./sleqrun` is the emulator that can run `.bin` binaries. These are simply loaded into the emulated memory at 0x0000, interpreted as a sequence of 16-bit little-endian words, and executed from there. The emulator has a 32-word region memory mapped to provide single-character I/O and a simple ALU. It halts upon encountering an instruction that jumps to itself with source and target operands matching, as that must always loop infinitely, and the emulator process exits with return value equal to the address of the src operand

`./hex1` is a basic "assembler" used to start the bootstrapping process. However, assembler is a bit generous, as all it does is convert ASCII hex-literals to binary, and ignores whitespace / single-line comments.

Running `make clean && make` should build the c components.
You can assemble and run a basic hello world program like so:
    $ ./hex1 <asm/hello.hex1 >hello.bin
        End of input: wrote 80 dwords (0x50)
    $ ./sleqrun hello.bin`
        Loading binary from hello.bin... 80 words
        ======= Running 16bit, 2MHz:
        Hello World
        ======= Halted with code 0 after 103 steps

The program gets in/output on stdin/stdout, sleqrun emulator prints debugging info on stderr,
For a full program trace, run with `./sleqrun --debug BINARY`

# Assembler Bootstrapping

While we began with a (hex)-assembler written in C, the goal is to eventuall have the subleq assemblers be self-hosting. The first step toward achieveing this is to replace our initial hex1 assembler (written in C), with a subleq program that does the same thing.

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


# Assembler upgrades

Now the assembler is self-hosting, but the code is raw machine code, all magic numbers, and can't be
reordered without changing the addresses on every line:
>   20 20 73
>   28 28 76
>    8 28 79
>   28 20 7C

I then write a series of upgraded assemblers to be able to
code in a slightly-less-awful environment:

- hex2 allows me to automatically pad the binary with nulls to move code to specific locations, so I no longer need to painstakingly put in an count nulls myself when making any change.
- hex3 allows me to specify simple PC-relative arithmetic in the assembly, making more code position independent (and allowing for somewhat ergonomic copy-pasting of pointer-related code).
- asm1 finally implements the ability to specify arbitrary string labels, making the code finally almost readable.
- asm2 adds support for inserting string literals into the binary, which are necessary for the next step.

Each one is more complex, and so uses the tools introduced by the previous iterations to implement itelf more cleanly, until finally we have something resembling normal assembly:

    read_label:
      Y Y;                  # Y (len)  := 0
      V V; tbl_end V; 4 V;  # V (-ptr) := -(tbl_end + 4)
                            # leaves 1 word for len field
    read_label_loop:        # (Tests if C is a valid label char)
      X X; C_ X;            # X (char) := +(next char)
      ...

(see details of assembler versions in [asm/README.md](asm/README.md))

# FORTH


Once I've made an assembler capable of string-labels, I can finally start working on making an ergonomic higher-level language ([FORTH](https://en.wikipedia.org/wiki/Forth_\(programming_language\))). Forth is designed for bootstrappability, and so after some finicky work implementing a minimal core for the language in subleq assembly (asm/sforth.asm2), the rest of the language bootstraps itself by interpreting source code written in itself (fth/sforth1.fth). Almost all the complex language features I implemented were able to be written without thinking about the assembly at all:


    : SHOWOFFSET ( addr - ; shows offsets like "4   (->3BFe)" )
        DUP @ 4 U.R
        DUP @ 0> IF ." (->" ELSE ." (<-" THEN
        DUP @ + ( jump_tgt ) 4 U.R ." )" ;

    : DIS1 ( addr -- )
        DUP SHOWADDR
        DUP ISUNSAFEPTR IF DROP ." unsafe\n" RETURN THEN
        DUP DS_OFFSET? IF
             ( addr ) SHOWOFFSET
        ELSE ( addr ) @ SHOW
        THEN ;

This is a much higher-level language, featuring IF/ELSE/THEN, loops, recursion, variables, string handling/formatting, and much easier pointer manipulation. I also have built much more powerful debugging tools, including an interactive REPL, memory / stack inspector, and disassembler.

```
[0 > : ISUNSAFEPTR ( n -- bool ) 8 33 WITHIN ; ( 0x8-0x20 are volatile )
OK
[0 > 100 ISUNSAFEPTR .  ( should be false )
0  OK
[0 > 20 ISUNSAFEPTR .   ( should be true )
1  OK
[0 > ' ISUNSAFEPTR HEX . ( print pointer in hex )
3DC8  OK
[0 > HEX 3DC8 20 HEXDUMP
              x0   x1   x2   x3   x4   x5   x6   x7        x8   x9   xA   xB   xC   xD   xE   xF  
       3DC0 : 4E   53   41   46   45   50   54   52    === 820  DFD  8    DFD  21   311F 880  3DBA
       3DD0 : 0    D    42   45   4C   4F   57   41    === 4C   4C   57   4F   52   44   53   820 
       3DE0 : DFD  FF01 DFD  200  311F 880  3DCF 0     === 8    2D   31   36   41   4C   49   47  
[0 > ' ISUNSAFEPTR SEE
    === 3DC8(ISUNSAFEPTR)
    3DC8 : 820 (_:_)
    3DC9 : DFD (LIT)
    3DCA : 8   ( 8 )
    3DCB : DFD (LIT)
    3DCC : 21  ('!')
    3DCD : 311F(WITHIN)
    3DCE : 880 (_;_)
 OK
```


<!-- Example interactive coding/debugging session
```
[0 > : SQR ( n ) DUP * ;
[0 > 3 sqr .
9  OK
[0 > 3 dup sqr .s
2 : 3 9  OK
[2 > 2drop
 OK
[0 > 3 dup sqr swap 1- .s   ( lets test this out )
2 : 9 2  OK
[2 > dup sqr swap 1- .s     ( we can see how the stack evolves over time)
3 : 9 4 1  OK
[3 > dup sqr swap 1- .s
4 : 9 4 1 0  OK
[4 > 2drop 2drop
 OK
[0 > : SUMSQRS ( n -- sum ) DUP 0= IF ( 0 ) RETURN THEN 
    DUP SQR SWAP 1- ( sum n ) RECURSE ;  
 OK
[0 > 3 sumqrs .s    ( oops, typo )
SUMQRS?
RESTART (v0.2)
[0 > 3 sumsqrs .s   ( oops, looks like we didn't sum the results )
4 : 9 4 1 0  OK
[4 > : SUMSQRS ( n -- sum ) DUP 0= IF ( 0 ) RETURN THEN 
    DUP SQR SWAP 1- ( sum n ) RECURSE ( sum recsum ) + ;
 OK
[4 > 2DROP 2DROP    ( we clean up our stack )
 OK
[0 > 3 sumsqrs .s
1 : 14  OK
```
-->


<!-- TODO
    - show language features (isprime?)
      LOOPS, IF/THEN, CASE, strings, errs, vars
    - show interactive session / REPL, .S, etc
    - show disassembler
        ' listprimes hex .
        ' listprimes see
        ' listprimes CFA> .
        470A 10 dump
    - show rs? useful for debugging recursive
      or tricky functions
        : T3 ( dumps the return stack) .RS ;
        : T2 T3 ;
        : T1 T2 ;
        T1
        === RSP: 1020
        3FC) 482C: (T2)+1   = [880 (_;_)]
        3FD) 4834: (T1)+1   = [880 (_;_)]
        3FE) 3752: (?EXECUTE)+12  = [880 (_;_)]
        3FF) 39CF: (NEW_INTERPRET)+10  = [3982(?STACK)]
        400) 38F9: (RESTART)+4A  = [E3A (LITSTR)]
    - recursion (RECURSE, TAILRECURSE)
: COLLATZ ( n ) DUP . ( print n )
CASE
    1 OF            .RS            ENDOF
    DUP 2 MOD 0= IF 2/     RECURSE ENDOF
    DEFAULT         3 * 1+ RECURSE ENDOF
    ENDCASE ;

: TAIL_COLL ( n ) DUP . ( print n )
CASE
    1 OF            .RS                     ENDOF
    DUP 2 MOD 0= IF 2/     TAILCALL RECURSE ENDOF
    DEFAULT         3 * 1+ TAILCALL RECURSE ENDOF
    ENDCASE ;

[0 > 3 collatz
    3 10 5 16 8 4 2 1 === RSP: 1014 
    3F6) 475E: (COLLATZ)+14  = [1560(BRANCH)]
    3F7) 475E: (COLLATZ)+14  = [1560(BRANCH)]
    3F8) 475E: (COLLATZ)+14  = [1560(BRANCH)]
    3F9) 475E: (COLLATZ)+14  = [1560(BRANCH)]
    3FA) 4769: (COLLATZ)+1F  = [1560(BRANCH)]
    3FB) 475E: (COLLATZ)+14  = [1560(BRANCH)]
    3FC) 4769: (COLLATZ)+1F  = [1560(BRANCH)]
    3FD) 3752: (?EXECUTE)+12  = [880 (_;_)]
    3FE) 39CF: (NEW_INTERPRET)+10  = [3982(?STACK)]
    3FF) 1E44: (INTERPRET)+249 = [880 (_;_)]
    400) 1E9B: (RESTART)+25  = [E3A (LITSTR)]
    OK
[0 > 3 tail_coll 
    3 10 5 16 8 4 2 1 === RSP: 1022 
    3FE) 3752: (?EXECUTE)+12  = [880 (_;_)]
    3FF) 39CF: (NEW_INTERPRET)+10  = [3982(?STACK)]
    400) 38F9: (RESTART)+4A  = [E3A (LITSTR)]
    OK



-->



<!-- Interactive session? Might be too long to work in easily
```
[0 > : DIVABLE ( n div -- b ) MOD 0= ;
 OK
[0 > 49 5 DIVABLE . 49 7 DIVABLE .
0 1 OK
 OK
[0 > 0 value n
 OK
[0 > 37 to n
 OK
[0 > : TRYDIVS ( stop start -- b ) DO N I> DIVABLE IF 1 LOOPRETURN THEN 2 +LOOP 0 ;
 OK
[0 > 10 3 trydivs . ( 37 is not divisible by any of 3..10)
0  OK
[0 > 49 to n   10 3 trydivs .  ( 49 is divisible by some of 3..10)
1 OK
[0 > 6 3 trydivs . ( 49 is not divisble by any of 3..6)
0  OK
[0 > : ISPRIME ( n ) TO N
    CASE
        DUP 4 < IF DROP 1 ENDOF
        ( oops, that's not right, let's retry ) XXXX
XXXX?
RESTART (v0.2)
[0 > : ISPRIME ( n ) TO N
    CASE
        N 4 < IF 1 ENDOF
        N 2 DIVABLE IF 0 ENDOF
        DEFAULT N 2/ 3 TRYDIVS 0= ENDOF
        ENDCASE ;
 OK
[0 > 2 isprime .
1  OK
[0 > 4 isprime . 7 isprime . 9 isprime .
0 1 0  OK
[0 > : LISTPRIMES ( stop start -- ) DO I> ISPRIME IF I> . THEN LOOP ;
 OK
[0 > 50 1 listprimes
1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47  OK
```
-->

