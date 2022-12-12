# Didn't have my laptop with me so typed on the phone. Couldn't find any haskell
# interpreter on the phone so this one's in python!

x = 1
c = 0
total = 0


def tick():
    global c, total
    c += 1
    if c % 40 == 20:
        total += c*x


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

print(total)
