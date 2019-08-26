#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <err.h>

#include "shared.h"
#include "hex.h"


//#define DEBUG_LOGGING

//design q: 16-bit word-addressed or byte-addressed? byte-addressed b/c irl memory modules
// (although could just chain two into a double-wide word-addressed mem)
// let's do 16-bit
//
// design choice: stdin/out: write character to it or subtract char from it
// sub is thematically consistent
// is set harder to implement in hw? is set useful?

// instruction is 2-byte addr A, 2-byte addr B, 2-byte addr NEXT


// DOCS:
// Program starts execution from 0x0
// Program halts if NEXT==? and A==B
// Return value is A
//
// Special addresses:
// Reading from a special address must be done with first operand. Using it as B will yield 0
// Writing to a special address must be done with second operand. Using it as A will yield 0
//  stdin: reading from here subtracts the next character from stdin
//  stdout: writing to here will write -*A to output
//
//  ALU A and B: normal registers, but contents determine ALU results
//  ALU &,|,^,>>,<<: contains A op B
//  >> doesn't fill in top negative bits



//struct vm_state defined in shared.h

struct vm_state global_vm;

#define IN_ADDR  8
#define OUT_ADDR 9
#define HALT_ADDR 0xD

//registers, (by convention)
#define X_ADDR 0x20
#define Y_ADDR 0x21
#define Z_ADDR 0x22
#define W_ADDR 0x23


#define ALU_A 0xA
#define ALU_B 0xB
#define ALU_BASE 0x10
#define ALU_MASK 0xFFF8 //top 5 bits, ALU gets 7 regs

#define ALU_AND 0x10
#define ALU_OR  0x11
#define ALU_XOR 0x12
#define ALU_LS  0x14
#define ALU_RS  0x15

//Test program: should output 'A' and exit ?
int16_t program_init[] = {
      //0x0 
      //Z     entry  Z2      P1   M1
       0,   0,0x20,   0,     1,  -1,   0,   5,

  // in,  out    A    B         0xD halt......
       0,   0,   0,   0,     0,   0,   0, 0xd,

      //0x10: ALU
    // &    |    ^          <<   >> 
       0,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,

      //0x20
//loop:
//    v1    A    ?   v2      B    ?   >>    Z
    0x40, 0xA,0x23,0x41,   0xB,0x26,0x15,   0,
   //?      Z  out    ?      Z    Z halt
    0x29,   0, 0x9,0x18,     0,   0, 0xD,   0,

     //0x30
       0,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,


      //0x40
    //v1   v2
 -0x4100,  -8,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,
};

//gets a char from stdin
int16_t get_input() {
    int16_t c = getc(stdin);
#ifdef DEBUG_LOGGING
    fprintf(stderr, "Input: '%c', %x\n", c & 0xFF, c);
#endif
    return c;
}

//clamps output to 8 bits, prints to stdout
void write_output(int16_t c) {
    putc(c & 0xFF, stdout);
#ifdef DEBUG_LOGGING
    if(c >= 0x20 && c <= 0x7E) {
        fprintf(stderr, "Output: '%c', %hx\n", c & 0xFF, c);
    } else {
        fprintf(stderr, "Output: nonprintable, %hx\n", c);
    }
#endif
}

#define SHOW_VAR(name,loc) fprintf(stderr, ", " #name "=%04hx", vm->mem[loc]);
#define SHOW_VAR_NEG(name,loc) fprintf(stderr, ", " #name "=%04hx (-%04hx)", vm->mem[loc], -vm->mem[loc]);

//Runs one step, returns 0 normally, 1 if halt
int step(struct vm_state *vm) {
    vm->num_cycles++;

#ifdef DEBUG_LOGGING
    fprintf(stderr, "DEBUG: at pc x%04hx", vm->pc);

    //optional debug printing
    SHOW_VAR_NEG(X,X_ADDR);
    SHOW_VAR_NEG(Y,Y_ADDR);

    SHOW_VAR(A,ALU_A);
    SHOW_VAR(B,ALU_B);
    SHOW_VAR(Z,0x0);
    //SHOW_VAR(T,0x3);
    SHOW_VAR_NEG(C,0x28);
    SHOW_VAR(N,0x29);
    SHOW_VAR(R,0x2B);

    fprintf(stderr, "\n");
#endif

    int16_t A =    vm->mem[vm->pc];
    int16_t B =    vm->mem[vm->pc+1];
    int16_t NEXT = vm->mem[vm->pc+2];

    // special cases:
    // 8 is stdin
    // 9 is stdout
    // if *A==8, read one character from stdin, subtract it from *B 
    // if *B==9, write -*B to stdout
    
    //Fetch vals
    int16_t A_VAL;
    //stdin
         if(A == IN_ADDR) { A_VAL = get_input(); } 
    //ALU ops
    else if((A & ALU_MASK) == ALU_BASE) {
        switch(A) {
            case(ALU_AND):  A_VAL = vm->mem[ALU_A] &  vm->mem[ALU_B]; break;
            case(ALU_OR ):  A_VAL = vm->mem[ALU_A] |  vm->mem[ALU_B]; break;
            case(ALU_XOR):  A_VAL = vm->mem[ALU_A] ^  vm->mem[ALU_B]; break;
            case(ALU_LS ):  A_VAL = vm->mem[ALU_A] << vm->mem[ALU_B]; break;
            case(ALU_RS ):  A_VAL = ((uint16_t)vm->mem[ALU_A]) >> vm->mem[ALU_B]; break;
            default:        A_VAL = 0;
        }
#ifdef DEBUG_LOGGING
        fprintf(stderr, "In ALU: result=%4hx\n", A_VAL);
#endif
    }
    //normal
    else                  { A_VAL = vm->mem[A]; }

    int16_t B_VAL = vm->mem[B];


    //Sub
    int16_t diff = B_VAL - A_VAL;

    //fprintf(stderr, "DEBUG:   ops: A=%hx, A_val=%hx,   B=%hx, B_val=%hx,  B_val2=%hx\n",
    //        A, A_VAL, B, B_VAL, diff);

    //Write back
    if(B == OUT_ADDR) { write_output(diff); }
    else       { vm->mem[B] = diff; }

    //Jump LEQ
    if(diff <= 0) {
        //HALT: halts on Z Z ?-1, operands are same means always 0, infinite loop is halt
        if(NEXT == vm->pc) {
            if(A == B) { return 1; }
        }

        vm->pc = NEXT;
    } else {
        vm->pc += 3;
    }

    return 0;
}


//Initializes vm_state struct
//clears all of memory
//pc = 0
void init_vm(struct vm_state *vm) {
    memset(vm->mem, 0, (MEM_SIZE + MEM_BUFF) * WORD_SIZE);
    vm->pc = 0;
    vm->num_cycles=0;
}

//Loads the hardcoded test program in program_init
void load_test_program(struct vm_state *vm) {
    memcpy(vm->mem, program_init, sizeof(program_init));
}

//Runs specified memory
//Assumes memory is of size (MEM_SIZE+MEM_BUFF)*WORD_SIZE
//Starts executing at pc=0, runs until halt
//
//On halt, returns retval = first operand of halt instruction
int run(struct vm_state *vm) {
    int retval;
    do {
        retval = step(vm);
    } while (retval == 0);

    //halted, get A operand of last instrucc
    return vm->mem[vm->pc];
}


//Reads bytes from file, slurps them into vm memory
void load_binary_file(struct vm_state *vm, FILE *file) {
    long offset=0;
    char *cmem = (char *)vm->mem;
    
    int c;
    while((c = getc(file)) != EOF) {
        if(offset >= MEM_SIZE * WORD_SIZE) {
            fprintf(stderr,"ERROR: binary > than %d words, exiting\n", MEM_SIZE);
            exit(1);
        }

        cmem[offset++] = c;
    }

    //check we got an even number of bytes
    if(offset % 2 == 1) {
        fprintf(stderr,"WARNING: malformed binary, has odd number of bytes\n");
        cmem[offset++] = 0;
    }

    //check nonempty
    if(offset == 0) {
        fprintf(stderr,"ERROR: empty binary, exiting\n");
        exit(1);
    }

    fprintf(stderr, "Loaded binary of %ld words\n", offset/2);
}

// ================= MAIN STUFF =================

void run_default_program() {
    printf("Initializing...\n");
    init_vm(&global_vm);
    load_test_program(&global_vm);
    printf("Running :\n=======\n");
    int retval = run(&global_vm);
    printf("=======\nExited with code %d\n", retval);
}

//loads a binary file and executes it
//returns return value
int run_binary(char *fname) {
    //arg 0 is script name, arg 1 is 'bin'
    
    FILE *binfile;
    if(strcmp(fname, "-") == 0) {
        binfile = stdin;
        fname = "stdin";
    } else {
        binfile = fopen(fname, "r");
    }

    if(binfile == NULL) { err(1, "Failed to open file %s", fname); }

    fprintf(stderr, "Loading binary from %s\n", fname);
    init_vm(&global_vm);
    load_binary_file(&global_vm, binfile);
    fprintf(stderr, "Running :\n=======\n");
    int retval = run(&global_vm);
    fprintf(stderr, "=======\nHalted with code %d after %ld steps\n", retval, global_vm.num_cycles);
    return(retval);
}

//print usage to stderr
void print_usage() {
    fprintf(stderr, "Usage: \n");
    fprintf(stderr, "      sleqrun -      # read binary from stdin until EOF, then run it\n");
    fprintf(stderr, "      sleqrun FILE   # read binary from file, run it\n");
}

int main(int argc, char *argv[]) {
    if(argc == 1) {
        print_usage(); 
        exit(1);
    } else if (argc >= 3) {
        fprintf(stderr, "ERROR: expected at most 1 arg\n");
        print_usage(); 
        exit(1);

    } else { //exactly 1 arg
        if(strcmp(argv[1], "--test") == 0) {
            //might be useful hook for later
            fprintf(stderr, "TEST MODE\n");
            exit(0);
        } else {
            int retcode=run_binary(argv[1]);
            exit(retcode);
        }
    }

}
