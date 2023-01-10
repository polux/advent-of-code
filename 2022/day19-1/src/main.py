from z3 import *
import re

def maximize(n, ore_for_ore, ore_for_clay, ore_for_obs, clay_for_obs, ore_for_geode, obs_for_geode):
    matrix = [
      [ore_for_ore, ore_for_clay, ore_for_obs , ore_for_geode],
      [0          , 0           , clay_for_obs, 0            ],
      [0          , 0           , 0           , obs_for_geode],
      [0          , 0           , 0           , 0            ]
    ]

    robots = [IntVector(f'robots_{i}',4) for i in range(n+1)]
    resources = [IntVector(f'resources_{i}',4) for i in range(n+1)]
    decisions = [IntVector(f'decisions_{i}',4) for i in range(n+1)]

    initRobots = [1,0,0,0]
    initResources = [0,0,0,0]

    o = Optimize()

    for j in range(4):
      robots[0][j] = initRobots[j]
      resources[0][j] = initResources[j]

    for i in range(1, n+1):
        o.add(Sum([decisions[i][j] for j in range(4)]) <= 1)
        for j in range(4):
          o.add(decisions[i][j] >= 0)
          o.add(decisions[i][j] <= 1)
          o.add(Implies(decisions[i][j] == 1, And([resources[i-1][k] >= matrix[k][j] for k in range(4)])))
          o.add(resources[i][j] == resources[i-1][j] + robots[i-1][j] - Sum([decisions[i][k] * matrix[j][k] for k in range(4)]))
          o.add(robots[i][j] == robots[i-1][j] + decisions[i][j])

    h = o.maximize(resources[n][3])
    o.check()
    return h.value().as_long()

def ints(l):
   return [int(s) for s in re.findall(r'\d+', l)]

res1 = 0
for l in open('input', 'r'):
    [n, *args] = ints(l)
    res1 += n * maximize(24, *args)
print(res1)

res2 = 1
for l in open('input', 'r').readlines()[:3]:
    [_, *args] = ints(l)
    res2 *= maximize(32, *args)
print(res2)
