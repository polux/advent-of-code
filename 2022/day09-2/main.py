# Didn't have my laptop with me so typed on the phone. Couldn't find any haskell
# interpreter on the phone so this one's in python!

from dataclasses import dataclass


@dataclass
class Coord:
    x: int
    y: int

    def move(self, dx, dy):
        self.x += dx
        self.y += dy


deltas = {
    'L': (-1, 0),
    'R': (1, 0),
    'D': (0, -1),
    'U': (0, 1)
}

seen = set([(0, 0)])
rope = [Coord(0, 0) for _ in range(10)]
head = rope[0]
tail = rope[9]


def touch(v1, v2):
    return (abs(v1.x-v2.x) <= 1
            and abs(v1.y-v2.y) <= 1)


def clamp(v):
    if v == 0:
        return 0
    elif v > 0:
        return 1
    else:
        return -1


s = open('input').read().strip()
for l in s.split('\n'):
    [cmd, ns] = l.split(' ')
    delta = deltas[cmd]
    n = int(ns)
    for _ in range(n):
        head.move(*delta)
        for i in range(1, 10):
            p = rope[i-1]
            v = rope[i]
            if not touch(p, v):
                v.move(
                    clamp(p.x-v.x),
                    clamp(p.y-v.y))
        seen.add((tail.x, tail.y))

print(len(seen))
