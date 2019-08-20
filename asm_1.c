#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "asm.h"


/* Design goals:
 * first binwrite will read hex from stdin, skipping whitespace and line comments
 * will write hex directly into binary format and write it to stdout
 *
 * each chunk is written as a 16-bit word in little-endian
 * reads 0-1A-F until a space or newline
 *
 * binwrite syntax:
 *   #comment
 *   1f 0 FFFF #some stuff
 *   2b 0 
 */

//reads from stdin to EOF
//writes binary to stdout
//on error, returns 1
//else returns 0
//prints compiled size
int asm_1() {
    int c;
    long offset = 0; //in dwords
    int curr_num = 0;
    char temp;


    //main loop: walks until it finds a number, then jumps to read_number
    while(1) {
        c = getchar();
        
        if(c == EOF) { goto end_input; }

        if(c == ' ') { continue; }
        if(c == '\n') { continue; }

        if(c == '#') { goto drop_comment; }

        if(c < '0')  { goto invalid_char; }
        if(c <= '9') { goto read_number; }
        if(c < 'A')  { goto invalid_char; }
        if(c <= 'F') { goto read_number; }

        //consumes numbers as long as it can, then outputs dword
read_number:
        if(c < '0')  { goto finish_num; }
        if(c <= '9') { c -= '0'; goto shift_up;}
        if(c < 'A')  { goto finish_num; }
        if(c <= 'F') { c -= 'A' - 10; goto shift_up; }
        if(c < 'a')  { goto finish_num; }
        if(c <= 'f') { c -= 'a' - 10; goto shift_up; }
        goto invalid_char;

        //shifts up, adds to curr_num, gets next char
        shift_up:
            // <<4 == *2, *2, *2, *2
            curr_num = curr_num + curr_num;
            curr_num = curr_num + curr_num;
            curr_num = curr_num + curr_num;
            curr_num = curr_num + curr_num;
            curr_num += c;
            c = getchar();
            goto read_number;

finish_num:
        //write out the number, little endian
        temp = curr_num & 0xFF;
        putchar(temp);
        temp = (curr_num >> 8) & 0xFF;
        putchar(temp);
        offset += 1;
        curr_num = 0;
        continue;


//reads chars until newline
drop_comment:
        c = getchar();
        if(c == '\n') { continue; }
        goto drop_comment;

        if(offset > MEM_SIZE) {
            fprintf(stderr, "ERROR: size exceeds limit of %d\n", MEM_SIZE);
            return 1;
        }
    }

    invalid_char:
        fprintf(stderr, "ERROR: invalid char '%c', %d\n", c, c);
        return 1;

    end_input:
        fprintf(stderr, "End of input: wrote %ld dwords (0x%lx)\n", offset, offset);
        return 0;
}

