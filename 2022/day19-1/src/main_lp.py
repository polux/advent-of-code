from pulp import *
import re

def maximize(n, ore_for_ore, ore_for_clay, ore_for_obs, clay_for_obs, ore_for_geode, obs_for_geode):
    matrix = [
      [ore_for_ore, ore_for_clay, ore_for_obs , ore_for_geode],
      [0          , 0           , clay_for_obs, 0            ],
      [0          , 0           , 0           , obs_for_geode],
      [0          , 0           , 0           , 0            ]
    ]

    TIME = range(n+1)
    RESOURCES = range(4)
    ROBOTS = range(4)

    robots = LpVariable.dicts("robots", (TIME, ROBOTS), cat="Integer", lowBound=0, upBound=n)
    resources = LpVariable.dicts("resources", (TIME, RESOURCES), cat="Integer", lowBound=0, upBound=n*n)
    decisions = LpVariable.dicts("decisions", (TIME, ROBOTS), cat="Binary")

    initRobots = [1,0,0,0]
    initResources = [0,0,0,0]

    o = pulp.LpProblem("production_problem", LpMaximize)
    # the variable to maximize
    o += resources[n][3]

    for j in range(4):
      o += (robots[0][j] == initRobots[j])
      o += (resources[0][j] == initResources[j])

    for i in range(1, n+1):
        o += lpSum([decisions[i][j] for j in range(4)]) <= 1
        for j in range(4):
          for k in range(4):
            o += (resources[i-1][k] >= decisions[i][j] * matrix[k][j])
          o += (resources[i][j] == resources[i-1][j] + robots[i-1][j] - lpSum([decisions[i][k] * matrix[j][k] for k in range(4)]))
          o += (robots[i][j] == robots[i-1][j] + decisions[i][j])

    o.solve(COIN_CMD(msg=False))
    return(int(resources[n][3].value()))

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
