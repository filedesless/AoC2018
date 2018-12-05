#!/usr/bin/env python3

lines = [ line.strip() for line in open('01-1.txt').readlines() ]

print(lines[:5])
print(sum(map(int, lines)))
