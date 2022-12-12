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
head = Coord(0, 0)
tail = Coord(0, 0)


def touch():
    return (abs(head.x-tail.x) <= 1
            and abs(head.y-tail.y) <= 1)


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
        if not touch():
            tail.move(
                clamp(head.x-tail.x),
                clamp(head.y-tail.y))
            seen.add((tail.x, tail.y))

print(len(seen))
