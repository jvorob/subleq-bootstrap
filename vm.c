#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "shared.h"
#include "asm.h"

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
// TODO: infinite loop detect?
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

#define ALU_A 0xA
#define ALU_B 0xB
#define ALU_BASE 0x10
#define ALU_MASK 0xF8 //top 5 bits, ALU gets 7 regs

#define ALU_AND 0x10
#define ALU_OR  0x11
#define ALU_XOR 0x12
#define ALU_LS  0x14
#define ALU_RS  0x15

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

int16_t program_init_hello[] = {
      //0x0 
      //Z     entry  Z2      P1   M1
       0,   0,0x20,   0,     1,  -1,   0,   5,

  // in,  out    A    B         0xD halt......
       0,   0,   0,   0,     0,   0,   0, 0xd,

      //0x10: ALU
    // &    |    ^          <<   >> 
       0,   0,   0,   0,     0,   0,   0,   0,
    // p    v
   -0x40,   0,   0,   0,     0,   0,   0,   0,

      //0x20
//loop:
//     V   V     ?    X      X  ?Row
    0x19,0x19,0x23,0x2b,  0x2b,0x28,   0,   0,

//     P    X    ?  X:_      V    ?    V    Z
    0x18,0x2b,0x2b,   0,  0x19,0x2e,0x19,   0,

//0x30
//  halt    Z    Z    ?      V  out    ?   P1
     0xD,   0,   0,0x34,  0x19,   9,0x37,   4,
       
//     P loop
    0x18,0x20,   0,   0,     0,   0,   0,   0,

      //0x40
     'H', 'e', 'l', 'l',   'o','\n',   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,
};

//gets a char from stdin
int16_t get_input() {
    return getc(stdin);
}

//clamps output to 8 bits, prints to stdout
void write_output(int16_t c) {
    //putc(c & 0xFF, stdout);
    printf("Output: '%c', %x\n", c & 0xFF, c);
}

//Runs one step, returns 0 normally, 1 if halt
int step(struct vm_state *vm) {
    printf("DEBUG: at pc x%04x, A=%x (%d), B=%x (%d)\n", 
            vm->pc, vm->mem[ALU_A], vm->mem[ALU_A], vm->mem[ALU_B], vm->mem[ALU_B]);

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
        printf("In ALU\n");
        switch(A) {
            case(ALU_AND):  A_VAL = vm->mem[ALU_A] &  vm->mem[ALU_B]; break;
            case(ALU_OR ):  A_VAL = vm->mem[ALU_A] |  vm->mem[ALU_B]; break;
            case(ALU_XOR):  A_VAL = vm->mem[ALU_A] ^  vm->mem[ALU_B]; break;
            case(ALU_LS ):  A_VAL = vm->mem[ALU_A] << vm->mem[ALU_B]; break;
            case(ALU_RS ):  A_VAL = vm->mem[ALU_A] >> vm->mem[ALU_B]; break;
            default:        A_VAL = 0;
        }
    }
    //normal
    else                  { A_VAL = vm->mem[A]; }

    int16_t B_VAL = vm->mem[B];

    //Sub
    int16_t diff = B_VAL - A_VAL;

    //Write back
    if(B == OUT_ADDR) { write_output(diff); }
    else       { vm->mem[B] = diff; }

    //Jump LEQ
    if(diff <= 0) {
        //HALT: halts on Z Z ?-1, operands are same means always 0, infinite loop is halt
        if(NEXT == vm->pc) {
            if(A == B) { printf("HALT\n"); return 1; }
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
    memcpy(vm->mem, program_init, sizeof(program_init));
    vm->pc = 0;
}

//Runs specified memory
//Assumes memory is of size (MEM_SIZE+MEM_BUFF)*WORD_SIZE
//Starts executing at pc=0, runs until halt
void run(struct vm_state *vm) {
    int retval;
    do {
        retval = step(vm);
    } while (retval == 0);
}

// ================= MAIN STUFF =================

void run_default_program() {
    printf("Initializing...\n");
    init_vm(&global_vm);
    printf("Running :\n=======\n");
    run(&global_vm);
    printf("=======\nExited\n");
}

//print usage to stderr
void print_usage() {
    printf("Usage: \n");
    printf("      subleq test # run default program \n");
    printf("      subleq asm1 # run asm1 on stdin \n");
}

int main(int argc, char *argv[]) {
    if(argc == 1) {
        //no args
        print_usage(); exit(0);
    } else {
        //argc >= 2, at least 1 args
        if(strcmp(argv[1], "test") == 0) {
            printf("Running default test program\n");
            run_default_program();
            exit(0);
        }
        else if(strcmp(argv[1], "asm1") == 0) {
            fprintf(stderr, "Assembling asm_1 \n");
            asm_1();
        }
        else {
            fprintf(stderr, "ERROR: Unknown argument '%s'\n", argv[1]);
            print_usage();
            exit(1);
        }
    }

}
