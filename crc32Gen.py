import os

crc32tab = []

with open("crc32.txt", 'r') as f:
  for line in f.readlines():
    line = line.strip('\n')
    print(line)



