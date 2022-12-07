s = open("input").read()

num_occurrences = [0 for _ in range(26)]
num_overflows = 0

for i in range(len(s)):
  add_index = ord(s[i]) - ord('a')
  current = num_occurrences[add_index]
  num_occurrences[add_index] += 1
  if current == 1:
    num_overflows += 1
  if i > 13:
    rm_index = ord(s[i-14]) - ord('a')
    current = num_occurrences[rm_index]
    num_occurrences[rm_index] -= 1
    if current == 2:
      num_overflows -= 1
    if num_overflows == 0:
      print(i+1)
      exit(0)

