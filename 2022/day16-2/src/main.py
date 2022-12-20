inpt =[(0,(0,[3,8,1])),(1,(13,[2,0])),(2,(2,[3,1])),(3,(20,[2,0,4])),(4,(3,[5,3])),(5,(0,[4,6])),(6,(0,[5,7])),(7,(22,[6])),(8,(0,[0,9])),(9,(21,[8]))]

graph = {}
cost = {}
for v,(c,vs) in inpt:
  cost[v] = c
  graph[v] = vs

print(graph)
V = len(graph)
C = 2**len(graph)
dp = [[[0 for _ in range(V)] for _ in range(C)] for _ in range(31)]

def yields(c):
  res = 0
  for i in range(V):
    if c & (1 << v):
      res += cost[v]
  return res


for c in range(C):
  for v in range(V):
    dp[0][c][v] = 0
for t in range(1,31):
  for c in range(C):
    m = 0
    for v in range(V):
      if not c & (1<<v):
        m = max(m, dp[t-1][c | (1<<v)][v])
      for n in graph[v]:
        m = max(m, dp[t-1][c][n])
      dp[t][c][v] = yields(c) + m
print(dp[30][0][0])