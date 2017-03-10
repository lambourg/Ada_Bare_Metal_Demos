#!/usr/bin/env python
from math import radians, sin

def gen_sin(x):
    val = sin(radians(x / 10.0))
    print '                  %d => %.10f,' % (x, val)

for d in range(0, 900):
    gen_sin(d)
