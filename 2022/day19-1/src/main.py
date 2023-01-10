from z3 import *
import re

def maximize(n, ore_for_ore, ore_for_clay, ore_for_obs, clay_for_obs, ore_for_geode, obs_for_geode):
    ore_robots = [Int(f"ore_robots_{i}") for i in range(n+1)]
    clay_robots = [Int(f"clay_robots_{i}") for i in range(n+1)]
    obs_robots = [Int(f"obs_robots_{i}") for i in range(n+1)]
    geode_robots = [Int(f"geode_robots_{i}") for i in range(n+1)]

    ore = [Int(f"ore_{i}") for i in range(n+1)]
    clay = [Int(f"clay_{i}") for i in range(n+1)]
    obs = [Int(f"obs_{i}") for i in range(n+1)]
    geode = [Int(f"geode_{i}") for i in range(n+1)]

    Decision, (mk_ore_robot, mk_clay_robot, mk_obs_robot, mk_geode_robot, mk_nothing) = EnumSort(
        'Decision', ('mk_ore_robot', 'mk_clay_robot', 'mk_obs_robot', 'mk_geode_robot', 'mk_nothing'))
    decision = [Const(f"decision_{i}", Decision) for i in range(n+1)]

    o = Optimize()

    o.add(ore_robots[0] == 1)
    o.add(clay_robots[0] == 0)
    o.add(obs_robots[0] == 0)
    o.add(geode_robots[0] == 0)

    o.add(ore[0] == 0)
    o.add(clay[0] == 0)
    o.add(obs[0] == 0)
    o.add(geode[0] == 0)

    for i in range(1, n+1):
        making_ore_robot = decision[i-1] == mk_ore_robot
        making_clay_robot = decision[i-1] == mk_clay_robot
        making_obs_robot = decision[i-1] == mk_obs_robot
        making_geode_robot = decision[i-1] == mk_geode_robot

        o.add(Implies(making_ore_robot, ore[i-1] >= ore_for_ore))
        o.add(Implies(making_clay_robot, ore[i-1] >= ore_for_clay))
        o.add(Implies(making_obs_robot, And(ore[i-1] >= ore_for_obs, clay[i-1] >= clay_for_obs)))
        o.add(Implies(making_geode_robot, And(ore[i-1] >= ore_for_geode, obs[i-1] >= obs_for_geode)))

        o.add(ore[i] == ore[i-1]
              + ore_robots[i-1]
              - If(making_ore_robot, ore_for_ore, 0)
              - If(making_clay_robot, ore_for_clay, 0)
              - If(making_obs_robot, ore_for_obs, 0)
              - If(making_geode_robot, ore_for_geode, 0))
        o.add(clay[i] == clay[i-1] + clay_robots[i-1] - If(making_obs_robot, clay_for_obs, 0))
        o.add(obs[i] == obs[i-1] + obs_robots[i-1] - If(making_geode_robot, obs_for_geode, 0))
        o.add(geode[i] == geode[i-1] + geode_robots[i-1])

        o.add(ore_robots[i] == ore_robots[i-1] + If(making_ore_robot, 1, 0))
        o.add(clay_robots[i] == clay_robots[i-1] + If(making_clay_robot, 1, 0))
        o.add(obs_robots[i] == obs_robots[i-1] + If(making_obs_robot, 1, 0))
        o.add(geode_robots[i] == geode_robots[i-1] + If(making_geode_robot, 1, 0))

    h = o.maximize(geode[n])
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
