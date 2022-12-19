ns = [811589153 * int(s) for s in open('input').read().split()]
N = len(ns)
next = [(i+1) % N for i in range(N)]
prev = [(i-1) % N for i in range(N)]
z_pos = ns.index(0)

def step(i):
  ni = next[i]
  nni = next[ni]
  pi = prev[i]
  next[pi] = ni
  prev[ni] = pi
  next[ni] = i
  prev[i] = ni
  next[i] = nni
  prev[nni] = i

def move_by(i, d):
  for _ in range(d):
    step(i)

def value_at(n):
  i = z_pos
  for _ in range(n % N):
    i = next[i]
  return ns[i]

for _ in range(10):
  for i in range(N):
    move_by(i, ns[i] % (N-1))
print(value_at(1000) + value_at(2000) + value_at(3000))