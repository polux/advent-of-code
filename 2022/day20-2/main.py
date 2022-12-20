ns = [811589153 * int(s) for s in open('input').read().split()]
N = len(ns)
next = [(i+1) % N for i in range(N)]
prev = [(i-1) % N for i in range(N)]
z_pos = ns.index(0)

def move_before(i, j):
  if i == prev[j]:
    return
  ni = next[i]
  pi = prev[i]
  pj = prev[j]
  next[pi] = ni
  prev[ni] = pi
  next[pj] = i
  prev[i] = pj
  next[i] = j
  prev[j] = i

def move_by(i, d):
  j = i
  for _ in range(d+1):
    j = next[j]
  move_before(i, j)

def value_at(n):
  i = z_pos
  for _ in range(n % N):
    i = next[i]
  return ns[i]

for _ in range(10):
  for i in range(N):
    move_by(i, ns[i] % (N-1))
print(value_at(1000) + value_at(2000) + value_at(3000))