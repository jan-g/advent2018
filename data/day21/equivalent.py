#!/usr/bin/env python3

import sys

r0 = int(sys.argv[1])

# while True:
#     r2 = 123
#     r2 &= 456
#     if not r2 == 72: continue
#     break
r0 = 0

r2 = 0
r2l = []
r2s = set()

"""
 4            seti 0 0 4            goto loc1
 5            seti 0 1 2            r2 = 0                                             r2 = 0
 6            bori 2 65536 5        loc6: r5 = r2 | 65536                       loc6:  r5 = r2 | (1<<16)
 7            seti 16123384 4 2     r2 = 16123384                                      r2 = 16123384
 8            bani 5 255 3          loc8: r3 = r5 & 255                         lob8:  r3 = r5 & 255
 9            addr 2 3 2            r2 += r3                                           r2 += r3
10            bani 2 16777215 2     r2 &= 16777215                                     r2 &= $FFFFFF
11            muli 2 65899 2        r2 *= 65889                                        r2 *= 65899
12            bani 2 16777215 2     r2 &= 16777215                                     r2 &= $FFFFFF
13            gtir 256 5 3          r3 = (256 > r5)                                    if r5 < 256:
14            addr 3 4 4            goto r3 : loc15 -> loc17 / loc16 -> loc28              goto loc28
15            addi 4 1 4            goto loc17                                         else: (fallthrough)
16            seti 27 6 4           goto loc28
"""

def loc17(r5):
    """
    Find r3 st. ((r3 + 1) << 8) > r5.
    if r5 = (R3 << 8 + x), 0 <= x < 256:
    then (R3 << 8) <= r5,
    and (R3 + 1) << 8 > r5
    ie, r3 = R3
    That is, the result is r5 >> 8.
    """
    return (r5 >> 8)
    """
    r3 = 0
    while True:  # loc18
        r1 = (r3 + 1) << 8
        if r1 <= r5:
            r3 += 1  # loc 24
            continue
        else:
            r5 = r3  # loc26
            break    # goto loc8
    return r5
    """

while True:   # loc6
    r5 = r2 | 65536
    # let the bytes of r2 be A,B,C (C lsb)
    # Then r5 = A|1,B,C
    r2 = 16123384 # 246,05,248
    while True:   # loc8
        # mix in each byte of r5 (r2' | 65536)
        r3 = r5 & 255
        r2 += r3
        r2 &= 16777215
        r2 *= 65899
        r2 &= 16777215
        if r5 < 256:
            # goto loc28
            r2l.append(r2)
            if r2 in r2s:
                print("first repeating value is", r2, "which occurs at", r2l.index(r2), "and", len(r2l) - 1)
                print("previous value is", r2l[-2])
                sys.exit(0)
            else:
                print(r2)
                pass
            r2s.add(r2)

            if r2 == r0:
                sys.exit(0)
            break        # goto loc6

        r5 >>= 8

"""
17            seti 0 3 3            loc17: r3 = 0                                      r3 = 0
18            addi 3 1 1            loc18: r3 += 1                             loc18:  r1 = r3 + 1
19            muli 1 256 1          r1 = r1 * 256                                      r1 <<= 8
20            gtrr 1 5 1            r1 = (r1 > r5)                                     if not r1 > r5:
21            addr 1 4 4            goto 22 -> 24 / 23                                     r1 = 0: goto loc24
22            addi 4 1 4            loc22: goto 24                                     else:
23            seti 25 6 4           loc23: goto 26                                         r1 = 1: goto loc26
24            addi 3 1 3            r3 += 1                                    loc24:  r3 += 1
25            seti 17 3 4           goto loc18                                         goto loc18
26            setr 3 8 5            loc26: r5 = r3                                     r5 = r3
27            seti 7 2 4            goto loc8                                          goto loc8


28            eqrr 2 0 3            loc28: r3 = (r2 = r0)                       loc28: if r2 == r0:
29            addr 3 4 4            goto r3 : 30 / halt                                    halt
30            seti 5 3 4            goto 6                                             goto loc6
"""
