#!/usr/bin/env python3

seen = set()
lines = [ line.strip() for line in open('01-1.txt').readlines() ]
freqs = list(map(int, lines))

i, v = 0, 0
while True:
    v += freqs[i % len(freqs)]
    if v in seen:
        break
    else:
        seen.add(v)
    print(seen, i, v)
    i += 1

print(v)
