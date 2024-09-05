#ifndef SHARED_H
#define SHARED_H

#include <stdint.h>

typedef char bool;
#define TRUE 1
#define FALSE 0

// stringify macros: lets you do e.g. `printf("My const:"STR(MYCONST)"\n");`)
#define STR_INNER(s) #s     // stringifies paramater
#define STR(s) STR_INNER(s) // expands outer macro once to its value


#define MEM_BUFF 20 //how much extra padding on the end, really should only need 2 words but whatever
#define WORD_SIZE 2
#define MEM_SIZE 65536

struct vm_state {
    int16_t mem[MEM_SIZE+MEM_BUFF]; //throw in a little extra in case we read past the end
    uint16_t pc; //NOTE: this needs to be a uint to index mem properly
    int64_t num_cycles;
};


#endif
