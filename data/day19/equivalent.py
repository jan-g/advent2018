#!/usr/bin/env pypy3

import sys

r3 = int(sys.argv[1])

print(sum(i for i in range(1, r3 + 1) if r3 % i == 0))