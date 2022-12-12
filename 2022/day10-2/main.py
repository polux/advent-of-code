# Didn't have my laptop with me so typed on the phone. Couldn't find any haskell
# interpreter on the phone so this one's in python!

x = 1
c = 0


def tick():
    global c, total
    c += 1
    pos = (c-1) % 40
    if pos == 0:
        print()
    print('#' if abs(pos-x) < 2 else '.', end='')


s = open('input').read().strip()
for l in s.split('\n'):
    op = l.split()
    cmd = op[0]
    if cmd == 'addx':
        tick()
        tick()
        x += int(op[1])
    elif cmd == 'noop':
        tick()
