Writing an emulator and tooling for the SUBLEQ architecture, a one-instruction computing arch

Goal is to write a series of bootstrapping assemblers in subleq, up to a useful macro language

# Execution:

To assemble a binary file, run
`grep <test.asm1 -v '#' | ./interp asm1 >test.bin`

To run a binary,
`./interp bin <test.bin`

Execution prints debugging info on stderr, program output on stdout


# ASSEMBLER VERSIONS

asm1: handles [0-1A-Fa-f] and whitespace
reads from stdin, writes to stdout
outputs all things as dwords, little-endian
can't handle comments, use `grep -v '#'`
