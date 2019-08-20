#ifndef SHARED_H
#define SHARED_H

#define MEM_BUFF 20 //how much extra padding on the end, really should only need 2 words but whatever
#define WORD_SIZE 2
#define MEM_SIZE 65536

struct vm_state {
    int16_t mem[MEM_SIZE+MEM_BUFF]; //throw in a little extra in case we read past the end
    int16_t pc;
};


#endif
