#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <err.h>

#include "shared.h"
#include "hex.h"

#ifdef __SYNTHESIS__
    #include "ap_utils.h"

    // If we're doing HLS Synthesis, we want to initialize memory with our desired SUBLEQ binary
    #include "../build/hardcoded_binary.h" //includes 'starting_code' and 'starting_code_length'
#endif /* __SYNTHESIS__ */





//design q: 16-bit word-addressed or byte-addressed? byte-addressed b/c irl memory modules
// (although could just chain two into a double-wide word-addressed mem)
// let's do 16-bit
//
// design choice: stdin/out: write character to it or subtract char from it
// sub is thematically consistent
// is set harder to implement in hw? is set useful?

// instruction is 2-byte addr A, 2-byte addr B, 2-byte addr NEXT


// DOCS:
// Basics: 
// Executing from PC
// if next 3 words at PC are A, B, NEXT
// do mem[B] -= mem[A]
// if(mem[B]<=0) pc = NEXT; else pc+=3
//
// CAVEAT:
//    do the assign before jumping, so that we can modify NEXT before jump
//
// CAVEAT:
//    we have a 64k memory space
//    addresses >32k will be negative numbers. therefore, interpret addresses as unsigned
//
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

// =============== GLOBALS
bool glb_DEBUGGING_ENABLED = FALSE;
bool glb_QUAD_ALIGNED = FALSE;  // if true, ops are 4 words at a time and must be 4-word aligned
struct vm_state global_vm;



// == UART STUFF
#define UART_OFFSET_ADDR 0
#define UART_RX   ((volatile int *)(axi_port + UART_OFFSET_ADDR ))
#define UART_TX   ((volatile int *)(axi_port + UART_OFFSET_ADDR  + 1))
#define UART_STAT ((volatile int *)(axi_port + UART_OFFSET_ADDR  + 2))
#define UART_CTRL ((volatile int *)(axi_port + UART_OFFSET_ADDR  + 3))

#define UART_FLAG_RX_RDY  0x1 // bit 0
#define UART_FLAG_TX_FULL 0x8 // bit 3
/*
 * NOTE on uart operation: RX and TX are both 8-bit
 *
 * CTRL reg is as follows:
 * bit 0: clear TX FIFO
 * bit 1: clear RX FIFO
 * bit 4: enable interrupt (0/1 = off/on)  //I think we keep this off for now
 *
 * STAT reg is as follows:
 * bit 0: RX FIFO has data
 * bit 1: RX FIFO full (dont care)
 * 
 * bit 2: TX FIFO empty (dont care)
 * bit 3: TX FIFO full
 *
 * bit 4: interrupts enabled
 *
 * == ERRORS: (cleared on read)
 * bit 5: rcv FIFO overflow
 * bit 6: Frame error
 * bit 7: Parity error (I think this is disabled)
 */


// =============== CONSTANTS
#define LOG_PREFIX "DEBUG-"


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


// ====================== FUNCS

//gets a char from stdin

int16_t get_input(volatile int *axi_port) {
#ifdef __SYNTHESIS__
    int u_state;
    do { // Keep waiting for input as long as RX buffer empty
        u_state = *UART_STAT;
    } while ((u_state & UART_FLAG_RX_RDY) == 0);
    return *UART_RX;
#else
    int16_t c = getc(stdin);
    if(glb_DEBUGGING_ENABLED) {
        if(c == '\n') {
            fprintf(stderr, LOG_PREFIX "Input: '\\n', %hx\n", c);
        } else if(c >= 0x20 && c <= 0x7E ) {
            fprintf(stderr, LOG_PREFIX "Input: '%c', %hx\n", c & 0xFF, c);
        } else {
            fprintf(stderr, LOG_PREFIX "Input: nonprintable, %hx\n", c);
        }
    }
    return c;
#endif
}

//clamps output to 8 bits, prints to stdout
void write_output(int16_t c, volatile int *axi_port) {
#ifdef __SYNTHESIS__
    int u_state;
    do { // Keep waiting as long as TX buffer full
        u_state = *UART_STAT;
    } while ((u_state & UART_FLAG_TX_FULL) != 0);
    *UART_TX = c;
#else
//void write_output(int16_t c) {
    putc(c & 0xFF, stdout);
    if(glb_DEBUGGING_ENABLED) {
        if(c == '\n') {
            fprintf(stderr, LOG_PREFIX "Output: '\\n', %hx\n", c);
        } else if(c >= 0x20 && c <= 0x7E ) {
            fprintf(stderr, LOG_PREFIX "Output: '%c', %hx\n", c & 0xFF, c);
        } else {
            fprintf(stderr, LOG_PREFIX "Output: nonprintable, %hx\n", c);
        }
    }
#endif 
}

#define SHOW_VAR(name,loc) fprintf(stderr, ", " #name "=%04hx", vm->mem[loc]);
#define SHOW_VAR_NEG(name,loc) fprintf(stderr, ", " #name "=%04hx (-%04hx)", vm->mem[loc], -vm->mem[loc]);
#define SHOW_VAR_SGN(name,loc) fprintf(stderr, ", " #name "=%c%04hx", vm->mem[loc]>0?'+':'-',vm->mem[loc]*(vm->mem[loc]>0?1:-1));

//Runs one step, returns 0 normally, 1 if halt
// axi_port is NULL for simulator, is used for HLS
int step(struct vm_state *vm, volatile int *axi_port) {
//-#-p-ra-g-m-a HLS PIPELINE

    vm->num_cycles++;

    if(glb_DEBUGGING_ENABLED){
        fprintf(stderr, LOG_PREFIX "OP: PC=x%04hx", vm->pc);

        //optional debug printing
        SHOW_VAR_NEG(X,X_ADDR);
        SHOW_VAR_NEG(Y,Y_ADDR);

        //SHOW_VAR(A,ALU_A);
        //SHOW_VAR(B,ALU_B);
        SHOW_VAR_SGN(P,0x22);
        SHOW_VAR_SGN(Q,0x23);
        //SHOW_VAR_NEG(TCnt,0x2C);
        //SHOW_VAR_NEG(TAB,0x800);
        //SHOW_VAR_NEG(TB2,0x802);
        //SHOW_VAR(Z,0x0);
        //SHOW_VAR(T,0x3);
        SHOW_VAR_NEG(R,0x27);
        //SHOW_VAR_NEG(C,0x28);
        //SHOW_VAR(N,0x29);

        fprintf(stderr, "\n");
    }


    // error out if trying to execute not quad-aligned op
    // TODO: maybe just round down instead?
    if(glb_QUAD_ALIGNED && ((vm->pc % 4) != 0)) {
        fprintf(stderr, "ERROR: trying to execute unaligned instruction at 0x%04hx in quad-aligned mode\n", vm->pc);
        return 1;
    }

    //NOTE: A and B are addresses, should be unsigned
    uint16_t A =    vm->mem[vm->pc];
    uint16_t B =    vm->mem[vm->pc+1];

    // special cases:
    // 8 is stdin
    // 9 is stdout
    // if *A==8, read one character from stdin, subtract it from *B 
    // if *B==9, write -*B to stdout
    
    // ==== Fetch vals
    //NOTE: A_VAL and B_VALS should be signed, since we do arithmetic/tests w/ them
    int16_t A_VAL;
    //stdin
         if(A == IN_ADDR) { A_VAL = get_input(axi_port); } 
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

        if(glb_DEBUGGING_ENABLED) {
            fprintf(stderr, LOG_PREFIX "ALU: result=%4hx\n", A_VAL);
        }
    }
    //normal
    else                  { A_VAL = vm->mem[A]; }

    int16_t B_VAL = vm->mem[B];


    // === SUBTRACT AND WRITE BACK
    int16_t diff = B_VAL - A_VAL;
    //fprintf(stderr, "DEBUG:   ops: A=%hx, A_val=%hx,   B=%hx, B_val=%hx,  B_val2=%hx\n",
    //        A, A_VAL, B, B_VAL, diff);
    
    if(B == OUT_ADDR) { write_output(diff, axi_port); }
    else       { vm->mem[B] = diff; }

    // === Advance PC 
    // (note: only fetch next after the modify)
    // (note2: actually this wont matter because if you want to jump to apointer
    // it will only work if it's <=0 (>32k?))
    // hmmm

    if(diff <= 0) {
        uint16_t NEXT = vm->mem[vm->pc+2];

        //HALT: halts on Z Z ?-1, operands are same means always 0, infinite loop is halt
        if(NEXT == vm->pc) { if(A == B) { return 1;} }

        vm->pc = NEXT;
    } else {

        vm->pc += glb_QUAD_ALIGNED? 4: 3;
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


#ifdef __SYNTHESIS__
int run(volatile int *axi_port) {
#else
int run() {
    int *axi_port = NULL; //pass this around to functions
#endif

#pragma HLS INTERFACE m_axi port=axi_port
#pragma HLS INTERFACE ap_ctrl_none port=return

#ifdef __SYNTHESIS__
    //TEMP: lets initialize the UART
    *UART_CTRL = 0x3 ; // clear RX FIFO and TX FIFO

     // Print hello world repeatedly
     char msg[] = "Hello World!\n----------\0";
     long i = 0;
     int p = 0;
     int u_state = 0;

     for(i = 0; i < 20000; i++) {
         //ap_wait();

         // do nothing while TX fifo is full
         do {
             u_state = *UART_STAT;
             //ap_wait();
         } while ((u_state & UART_FLAG_TX_FULL) != 0);

         *UART_TX = msg[p];
         p++;

         if (msg[p] == '\0') {
             p = 0;
         }

     }

    return 0;
#else

    int retval;
    struct vm_state *vm = &global_vm;
    do {
        retval = step(vm, axi_port);
    } while (retval == 0);
    return vm->mem[vm->pc];
#endif /* __SYNTHESIS__ */



    //halted, get A operand of last instrucc
}



#ifndef __SYNTHESIS__

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
    printf("Running %s:\n=======\n", glb_QUAD_ALIGNED? "quad-aligned":"unaligned");
    int retval = run();
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
    fprintf(stderr, "Running %s:\n=======\n", glb_QUAD_ALIGNED? "quad-aligned":"unaligned");
    int retval = run();
    fprintf(stderr, "=======\nHalted with code %d after %ld steps\n", retval, global_vm.num_cycles);
    return(retval);
}

//print usage to stderr
void print_usage() {
    fprintf(stderr, "Usage: \n");
    fprintf(stderr, "      sleqrun -      # read binary from stdin until EOF, then run it\n");
    fprintf(stderr, "      sleqrun FILE   # read binary from file, run it\n");
    fprintf(stderr, "   Options:\n");
    fprintf(stderr, "      --debug        # enables debugging output\n");
    fprintf(stderr, "      --4aligned     # instructions must be 4-word aligned, 4th is ignored\n");
    fprintf(stderr, "      --unaligned    # instructions are any 3 consecutive dwords\n");
    fprintf(stderr, "                     # (default is unaligned)\n");
}


// Arg Parsing stuff
// options 

int main(int argc, char *argv[]) {


    // ========== HANDLE OPTION FLAGS
    //discards processed opts, shifting argv down
    int read_i = 1;  //start at first arg
    int shift_amount = 0;
    for(; read_i < argc; read_i++) {

        //copy this arg down to account for removed args
        argv[read_i - shift_amount] = argv[read_i]; 
        //printf("Arg %d: %s\n", read_i, argv[read_i]);
        
        //long opts
        char *curr_arg = argv[read_i];
        if( curr_arg[0] == '-' && curr_arg[1] == '-') {

            if(0 == strcmp(curr_arg, "--debug")) {
                glb_DEBUGGING_ENABLED = TRUE;
            } else if(0 == strcmp(curr_arg, "--4aligned")) {
                glb_QUAD_ALIGNED = TRUE;
            } else if(0 == strcmp(curr_arg, "--unaligned")) {
                glb_QUAD_ALIGNED = FALSE;
            } else {
                fprintf(stderr, "ERROR: Unknown option '%s'\n", curr_arg);
                print_usage();
                exit(1);
            }

            shift_amount++; //shift down
        }
    }
    argc -= shift_amount; //update argc to account for removed elements

    //printf("\nHandled -- args, showing updated args\n");
    //for(int i = 1; i < argc; i++) {
    //    printf("Arg %d: %s\n", i, argv[i]);
    //}


    // ================ PARSE POSITIONAL FLAGS
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

#endif /* __SYNTHESIS__ */
