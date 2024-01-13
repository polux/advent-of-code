from collections import defaultdict
from dataclasses import dataclass
from functools import reduce
from random import shuffle
import operator

def parse(lines):
  vertices = set()
  edges = []
  for l in lines:
    [src, rhs] = l.strip().split(": ")
    targets = rhs.split(" ")
    for tgt in targets:
      vertices.add(src)
      vertices.add(tgt)
      edges.append((src, tgt))
  return (vertices, edges)

@dataclass
class DisjointSet:
  parent: str

def karger(vertices, edges):
  sets = {v: DisjointSet(v) for v in vertices}

  def find(x):
    x_set = sets[x]
    if x_set.parent != x:
      x_set.parent = find(x_set.parent)
    return x_set.parent

  def fuse(x, y):
    sets[find(y)].parent = find(x)

  shuffle(edges)
  num_vertices_left = len(vertices)
  for (src, tgt) in edges:
    if find(src) == find(tgt):
      continue
    else:
      fuse(src, tgt)
      num_vertices_left -= 1
    if num_vertices_left == 2:
      break

  min_cut = 0
  for (src, tgt) in edges:
    if find(src) != find(tgt):
      min_cut += 1

  return (min_cut, find)

(vertices, edges) = parse(open("input"))
while True:
  (min_cut, find) = karger(vertices, edges)
  if min_cut == 3:
    counts = defaultdict(lambda: 0)
    for v in vertices:
      counts[find(v)] += 1
    print(reduce(operator.mul, counts.values(), 1))
    break



