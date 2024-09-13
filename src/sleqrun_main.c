#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <err.h>
#include <time.h>

#include "shared.h"
#include "hex.h"

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

struct vm_debug_opts {
    int dump_base_addr;
    int dump_len;
};

// =============== GLOBALS
bool glb_QUAD_ALIGNED = FALSE;  // if true, ops are 4 words at a time and must be 4-word aligned
struct vm_state global_vm;

// =============== DEBUGGING GLOBALS
bool glb_DEBUGGING_ENABLED = FALSE;

struct vm_debug_opts glb_debug_opts = {0};


// =============== CONSTANTS
#define LOG_PREFIX "DEBUG-"

#define DEFAULT_MHZ 2

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
#define ALU_MASK 0xFFF8 // ignore bottom 3 bits, ALU gets 8 regs

#define ALU_AND 0x10
#define ALU_OR  0x11
#define ALU_XOR 0x12
#define ALU_LS  0x14
#define ALU_RS  0x15
#define ALU_ARS  0x16

//Test program: should output 'A' and exit ?
int16_t program_init[] = {
      //0x0
      //Z     entry  Z2      P1   M1
       0,   0,0x20,   0,     1,  -1,   0,   5,

  // in,  out    A    B         0xD halt......
       0,   0,   0,   0,     0,   0,   0, 0xd,

      //0x10: ALU
    // &    |    ^          <<   >>  +>>
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


// ============= THROTTLE STUFF
//
//
// Call throttle_init once
// Call throttle_consume_ms(n) to delay that long (accurately!)
// Call throttle_mayblock_start() and _end() whenever target system
//      ISNT actively running, so we dont grant it that wallclock time towards
//      its runtime budget in throttle_consume

const clockid_t CLOCK = CLOCK_MONOTONIC;
struct timespec throttle_last_time = { } ;

void throttle_init() {
    // Get initial time, we'll use this to throttle our progress
    clock_gettime(CLOCK, &throttle_last_time);
}


const int64_t MSEC_TO_NS = 1000 * 1000;
const int64_t SEC_TO_NS = 1000 * MSEC_TO_NS;

void time_add_ns(struct timespec *t, long ns) {
    t->tv_nsec += ns;
         if ( t->tv_nsec > SEC_TO_NS) { t->tv_sec++; t->tv_nsec -= SEC_TO_NS; }
    else if ( t->tv_nsec < 0        ) { t->tv_sec--; t->tv_nsec += SEC_TO_NS; }
}
void time_sub_ns(struct timespec *t, long ns) { time_add_ns(t, -ns); }

void time_add(struct timespec *a, const struct timespec *b) {
    time_add_ns(a, b->tv_nsec); // add the nanoseconds and carry
    a->tv_sec += b->tv_sec;
}
void time_sub(struct timespec *a, const struct timespec *b) {
    time_sub_ns(a, b->tv_nsec); // subtract the nanoseconds and carry
    a->tv_sec -= b->tv_sec;
}


void throttle_consume_ns(long ns) {
    time_add_ns(&throttle_last_time, ns);

    // Sleep until that time
    clock_nanosleep(CLOCK, TIMER_ABSTIME, // sleep until that time
            &throttle_last_time, NULL );
}

void throttle_consume_ms(long ms) { throttle_consume_ns(ms * MSEC_TO_NS); }

struct timespec throttle_block_start = { } ;
// If we have a blocking operation, we want to count that time as not having elapsed
void throttle_mayblock_start() {
    clock_gettime(CLOCK, &throttle_block_start);
}
void throttle_mayblock_end() {
    struct timespec throttle_block_end = { } ;
    clock_gettime(CLOCK, &throttle_block_end);

    // Do end - start for the difference
    time_sub(&throttle_block_end, &throttle_block_start);

    // Add the difference into throtle_last_time, so any time spent blocked wont countdown for instruction throttling
    time_add(&throttle_last_time, &throttle_block_end);
}
// ============= END THROTTLE


// ====================== FUNCS

#include <string.h> /* for memcpy() */
#include <termios.h>

void set_unbuffered() {
    // changes stdin/out to be unbuffered
    setvbuf(stdin, NULL, _IONBF, 0);
    setvbuf(stdout, NULL, _IONBF, 0);

    struct termios term_stored;
    struct termios term_new;
    tcgetattr(0,&term_stored); // save old value
    memcpy(&term_new,&term_stored,sizeof(struct termios));
    term_new.c_lflag &= ~(ECHO|ICANON); /* disable echo and canonization */
    tcsetattr(0,TCSANOW,&term_new);

    //tcsetattr(0,TCSANOW,&term_stored); /* restore the original state */
}

//gets a char from stdin
int16_t get_input() {
    // getc mightblock, and we don't to count that time towards our throttle limit
    // (since the SUBLEQ CPU couldn't be executing ops then)
    throttle_mayblock_start();
    int16_t c = getc(stdin);
    throttle_mayblock_end();

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
}

//clamps output to 8 bits, prints to stdout
void write_output(int16_t c) {
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
}

#define SHOW_VAR(name,loc) fprintf(stderr, ", " #name "=%04hx", vm->mem[loc]);
#define SHOW_VAR_NEG(name,loc) fprintf(stderr, ", " #name "=%04hx (-%04hx)", vm->mem[loc], -vm->mem[loc]);
#define SHOW_VAR_SGN(name,loc) fprintf(stderr, ", " #name "=%c%04hx", vm->mem[loc]>0?'+':'-',vm->mem[loc]*(vm->mem[loc]>0?1:-1));

//Runs one step, returns 0 normally, 1 if halt
int step(struct vm_state *vm) {
    vm->num_cycles++;

    if(glb_DEBUGGING_ENABLED){
        fprintf(stderr, LOG_PREFIX "OP: PC=x%04hx", vm->pc);

        //optional debug printing

        // === Standard asm variables, useful for debugging assembler
        SHOW_VAR_NEG(X,X_ADDR);
        SHOW_VAR_NEG(Y,Y_ADDR);
        SHOW_VAR_SGN(V, 0x22);
        SHOW_VAR_SGN(W, 0x23);
        SHOW_VAR_NEG(P1, 0x24);

        // SHOW_VAR(sb,0x38);
        // SHOW_VAR(se,0x39);


        // === Forth vars
        SHOW_VAR(NWA, 0x800);
        //SHOW_VAR(CWA, 0x810);
        //SHOW_VAR(TOS, 0x180);
        //SHOW_VAR_NEG(NOSP, 0x181);
        //SHOW_VAR_NEG(RSP, 0x182);

        // === Other?
        //SHOW_VAR(A,ALU_A);
        //SHOW_VAR(B,ALU_B);
        //SHOW_VAR_SGN(P,0x22);
        //SHOW_VAR_SGN(Q,0x23);
        //SHOW_VAR_NEG(TCnt,0x2C);
        //SHOW_VAR_NEG(TAB,0x800);
        //SHOW_VAR_NEG(TB2,0x802);
        //SHOW_VAR(Z,0x0);
        SHOW_VAR(T,0x3);
        SHOW_VAR_NEG(P2, 0x25);
        //SHOW_VAR_NEG(R,0x27);
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
         if(A == IN_ADDR) { A_VAL = get_input(); }
    //ALU ops
    else if((A & ALU_MASK) == ALU_BASE) {
        switch(A) {
            case(ALU_AND):  A_VAL = vm->mem[ALU_A] &  vm->mem[ALU_B]; break;
            case(ALU_OR ):  A_VAL = vm->mem[ALU_A] |  vm->mem[ALU_B]; break;
            case(ALU_XOR):  A_VAL = vm->mem[ALU_A] ^  vm->mem[ALU_B]; break;
            case(ALU_LS ):  A_VAL = vm->mem[ALU_A] << vm->mem[ALU_B]; break;
            case(ALU_RS ):  A_VAL = ((uint16_t)vm->mem[ALU_A]) >> vm->mem[ALU_B]; break;
            case(ALU_ARS):  A_VAL = vm->mem[ALU_A] >> vm->mem[ALU_B]; break;
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

    if(B == OUT_ADDR) { write_output(diff); }
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
//if throttle_mhz > 0, limits speed to that many MHz
//
//On halt, returns retval = first operand of halt instruction
int run(struct vm_state *vm, int throttle_mhz) {
    int retval;

    throttle_init();
    // (NOTE: I checked on acetate, throttling is within 1% accurate up to ~100MHz)
    // ( however, unthrottled it seems to cap out at around 120MHz on my machine,
    //   so that's about the limit )
    if (throttle_mhz <= 0) { throttle_mhz = 1000000; } // effectively unlimited
    const int64_t ops_per_msec = throttle_mhz * 1000; // 1MHZ is 1000 ops/ms

    do {
        retval = step(vm); // RUN a step

        // Handle throttling
        if (vm->num_cycles % ops_per_msec == 0) { throttle_consume_ms(1); }

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

    fprintf(stderr, "%ld words\n", offset/2);
}

// ================= MAIN STUFF =================

void run_default_program() {
    printf("Initializing...\n");
    init_vm(&global_vm);
    load_test_program(&global_vm);
    printf("Running %s:\n=======\n", glb_QUAD_ALIGNED? "quad-aligned":"unaligned");
    int retval = run(&global_vm, -1);
    printf("=======\nExited with code %d\n", retval);
}

//loads a binary file and executes it
//returns return value
int run_binary(char *fname, int throttle_mhz) {
    //arg 0 is script name, arg 1 is 'bin'

    // TODO: TEMP
    //set_unbuffered();

    FILE *binfile;
    if(strcmp(fname, "-") == 0) {
        binfile = stdin;
        fname = "stdin";
    } else {
        binfile = fopen(fname, "r");
    }

    if(binfile == NULL) { err(1, "Failed to open file %s", fname); }

    fprintf(stderr, "Loading binary from %s... ", fname);
    init_vm(&global_vm);
    load_binary_file(&global_vm, binfile);

    fprintf(stderr, "======= Running %s, %dMHz:\n",
                glb_QUAD_ALIGNED? "quad-aligned":"16bit",
                throttle_mhz);
    int retval = run(&global_vm, throttle_mhz);
    fprintf(stderr, "======= Halted with code %d after %ld steps\n",
                retval, global_vm.num_cycles);

    if (glb_debug_opts.dump_len > 0) {
        int addr = glb_debug_opts.dump_base_addr;
        int len = glb_debug_opts.dump_len;
        fprintf(stderr, "Debug: dumping memory at %04hx: 0x%hx words\n", addr, len);

        for (int i = 0; i < len; i++) {
            int curr_addr = addr + i;
            fprintf(stderr, "--  %04hx: %04hx (-%04hx)\n",
                            curr_addr, global_vm.mem[curr_addr], -global_vm.mem[curr_addr]);
        }
    }

    return(retval);
}

//print usage to stderr
void print_usage() {
    fprintf(stderr, "Usage: \n");
    fprintf(stderr, "      sleqrun -       # read binary from stdin until EOF, then run it\n");
    fprintf(stderr, "      sleqrun FILE    # read binary from file, run it\n");
    fprintf(stderr, "   Run options:\n");
    fprintf(stderr, "      --4aligned      # instructions must be 4-word aligned, 4th is ignored\n");
    fprintf(stderr, "      --unaligned     # instructions are any 3 consecutive dwords\n");
    fprintf(stderr, "                      # (default is unaligned)\n");
    fprintf(stderr, "      --mhz N         # Throttle speed to N mhz (1-100, default "
                                             STR(DEFAULT_MHZ) "Mhz)\n");
    fprintf(stderr, "   Debugging Options:\n");
    fprintf(stderr, "      --debug         # enables verbose debugging output per-step\n");
    fprintf(stderr, "      --dump ADDR LEN # on program exit, dumps LEN words at ADDR\n");
}


// parses string str, puts result in res
// returns 1 if succeeded, 0 if failed
int parse_hex(char *str, int *result) {
    if (str[0] == '\0') { return 0; }
    char *endptr;
    *result = strtol(str, &endptr, 16);

    // endptr now is how much of the string was parsed
    // success if whole string was valid number, so endptr == '\0'
    return *endptr == '\0';
}

// parses string str, puts result in res
// returns 1 if succeeded, 0 if failed
int parse_int(char *str, int *result) {
    if (str[0] == '\0') { return 0; }
    char *endptr;
    *result = strtol(str, &endptr, 10);

    // endptr now is how much of the string was parsed
    // success if whole string was valid number, so endptr == '\0'
    return *endptr == '\0';
}

// Arg Parsing stuff
// options

int main(int argc, char *argv[]) {
    int throttle_mhz = DEFAULT_MHZ; // default

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
            } else if(0 == strcmp(curr_arg, "--mhz")) {
                // Grab one opt
                if (argc - read_i - 1 < 1) {
                    fprintf(stderr, "ERROR: --mhz requires 1 arg\n");
                    print_usage();
                    exit(1);
                }
                // parse args
                int mhz_arg = -1;
                if (! parse_int(argv[read_i+1], &mhz_arg)) {
                    fprintf(stderr, "ERROR: --dump: failed to parse hex arg '%s'\n", argv[read_i+1]);
                    print_usage(); exit(1);
                }

                throttle_mhz = mhz_arg;

                // advance pointers
                read_i += 1;
                shift_amount += 1;

            } else if(0 == strcmp(curr_arg, "--dump")) {
                // Grab two opts
                if (argc - read_i - 1 < 2) {
                    fprintf(stderr, "ERROR: --dump requires 2 args\n");
                    print_usage();
                    exit(1);
                }

                // parse args
                int dump_arg_1 = -1;
                int dump_arg_2 = -1;
                if (! parse_hex(argv[read_i+1], &dump_arg_1)) {
                    fprintf(stderr, "ERROR: --dump: failed to parse hex arg '%s'\n", argv[read_i+1]);
                    print_usage();
                    exit(1);
                }
                if (! parse_hex(argv[read_i+2], &dump_arg_2)) {
                    fprintf(stderr, "ERROR: --dump: failed to parse hex arg '%s'\n", argv[read_i+2]);
                    print_usage();
                    exit(1);
                }

                // set the args: base, len
                glb_debug_opts.dump_base_addr = dump_arg_1;
                glb_debug_opts.dump_len =       dump_arg_2;

                // advance pointers
                read_i += 2;
                shift_amount += 2;

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
            int retcode=run_binary(argv[1], throttle_mhz);
            exit(retcode);
        }
    }

}
