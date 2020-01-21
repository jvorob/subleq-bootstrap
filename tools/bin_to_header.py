#!/usr/bin/python3

import sys


def errExit(msg, code):
    print(msg, file=sys.stderr)
    sys.exit(code)
    

args = sys.argv[1:]

if len(args) == 0:
    errExit(1, "ERR: No args")
    
if len(args) > 1:
    errExit(1, "ERR: Too many args")


filename = args[0]


nums = [] #contains a string like '0x00ab' for every word in the binary

with open(filename, "rb") as f:
    while True:
        b = f.read(2)

        if len(b) == 0:
            break

        elif len(b) == 2:
            #parse them
            val = int.from_bytes(b, byteorder='little', signed=False)
            nums.append("0x{:04x}".format(val))

        elif len(b) == 1:
            print("Error: file ends on odd number of bytes" ,file=sys.stderr)
            sys.exit(1)

        else:
            print("????", file=sys.stderr)
            sys.exit(1)


print("int16_t starting_code[] = {")

ints_per_line = 16
offset = 0

while len(nums) - offset > 0:
    line_str = "/* 0x{:04x}: */".format(offset)

    line_data = nums[offset:offset+ints_per_line]
    line_str += "  " + ",".join(line_data)
    offset += len(line_data)

    if(len(nums) - offset > 0):
        line_str = line_str + ","


    print(line_str)


print("};")
print("const uint16_t starting_code_length = 0x{:x};".format(offset))
