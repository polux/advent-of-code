# Didn't have my laptop with me so typed on the phone. Couldn't find any haskell
# interpreter on the phone so this one's in python!

from dataclasses import dataclass
from collections import deque
from typing import Deque
from typing import Callable


@dataclass
class Monkey:
    queue: Deque[int]
    operation: Callable[[int], int]
    condition: int
    ifTrue: int
    ifFalse: int
    inspections: int


def parse(s):
    return [parseMonkey(c) for c in s.split('\n\n')]


def parseMonkey(s):
    ls = s.split('\n')
    queue = deque(map(int, ls[1].split(':')[1].split(',')))
    def operation(old): return eval(ls[2].split('=')[1])
    condition = int(ls[3].split()[-1])
    ifTrue = int(ls[4].split()[-1])
    ifFalse = int(ls[5].split()[-1])
    return Monkey(queue, operation, condition, ifTrue, ifFalse, 0)


monkeys = parse(open('input').read().strip())
for _ in range(20):
    for m in monkeys:
        m.inspections += len(m.queue)
        while m.queue:
            i = m.queue.pop()
            val = m.operation(i) // 3
            m2i = m.ifTrue if val % m.condition == 0 else m.ifFalse
            monkeys[m2i].queue.appendleft(val)

res = [m.inspections for m in monkeys]
res.sort()
print(res[-1]*res[-2])
