#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

//design q: 16-bit word-addressed or byte-addressed? byte-addressed b/c irl memory modules
// (although could just chain two into a double-wide word-addressed mem)
// let's do 16-bit
//
// design choice: stdin/out: write character to it or subtract char from it
// sub is thematically consistent
// is set harder to implement in hw? is set useful?

// instruction is 2-byte addr A, 2-byte addr B, 2-byte addr NEXT



#define MEM_SIZE 65536
#define MEM_BUFF 20 //how much extra padding on the end, really should only need 2 words but whatever
#define WORD_SIZE 2



struct vm_state {
    int16_t mem[MEM_SIZE+MEM_BUFF]; //throw in a little extra in case we read past the end
    int16_t pc;
};

struct vm_state global_vm;

int16_t program_init[] = {
      //0x0 
      //Z     entry  Z2      P1   M1
       0,   0,0x20,   0,     1,  -1,   0,   5,

  // in,  out                   0xD halt......
       0,   0,   0,   0,     0,   0,   0, 0xd,

      //0x10: vars
    // p    v
   -0x40,   0,   0,   0,     0,   0,   0,   0,
       0,   0,   0,   0,     0,   0,   0,   0,

      //0x20
//loop:
//     V   V     ?    X      X  ?Row
    0x11,0x11,0x23,0x2b,  0x2b,0x28,   0,   0,

//     P    X    ?  X:_      V    ?    V    Z
    0x10,0x2b,0x2b,   0,  0x11,0x2e,0x11,   0,

//0x30
//  halt    Z    Z    ?      V  out    ?   P1
     0xD,   0,   0,0x34,  0x11,   9,0x37,   4,
       
//     P loop
    0x10,0x20,   0,   0,     0,   0,   0,   0,

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
    printf("Output: '%c'\n", c & 0xFF);
}

//Runs one step, returns 0 normally, 1 if halt
int step(struct vm_state *vm) {
    printf("DEBUG: at pc %x, p=%x, v=%d\n", vm->pc, vm->mem[0x10], vm->mem[0x11]);

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
    if(A == 8) { A_VAL = get_input(); } 
    else       { A_VAL = vm->mem[A]; }

    int16_t B_VAL = vm->mem[B];

    //Sub
    int16_t diff = B_VAL - A_VAL;

    //Write back
    if(B == 9) { write_output(diff); }
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



int main(int argc, char *argv[]) {
    printf("Initializing...\n");

    init_vm(&global_vm);

    printf("Running :\n=======\n");

    run(&global_vm);

    printf("=======\nExited\n");
}
