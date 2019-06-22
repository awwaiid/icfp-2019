#!/usr/bin/env python
import subprocess
import json

engine = subprocess.Popen(
    './game_engine/engine.native',
    stdout=subprocess.PIPE,
    stdin=subprocess.PIPE,
    bufsize=0)

task_json = subprocess.check_output(["./bin/parse-task", "./data/prob-001.desc"], universal_newlines=True)

engine.stdin.write(task_json.encode())
engine.stdin.write(b'\n')
result = engine.stdout.readline()
print(result.decode())

engine.stdin.write(b'{ "cmd": "get_state" }\n')
result = engine.stdout.readline()
print(result.decode())

# engine.stdin.write(b'{ "cmd": "action", "action": "W" }\n')
# result = engine.stdout.readline()
# print(result.decode())

data = json.loads(result.decode())
mapList = data["map"]
seen = []
seen_count = 0
for i in mapList:
    for j in i:
        if j != "W" and j != "O":
            seen_count += 1

print("there are " + str(seen_count) + " tiles to paint\n")
current = data["bot_position"]
direction = [1, 0, 'D']
manipulators = {
    "D": [[1, 0], [1, 1], [1, -1]],
    "W": [[0, 1], [-1, 1], [1, 1]],
    "A": [[-1, 0], [-1, -1], [-1, 1]],
    "S": [[0, -1], [1, -1], [-1, -1]],
}
moves = []


def check_manipulators(pos, dir):
    for i in manipulators[dir[2]]:
        if pos[0] + i[0] < 0 or pos[1] + i[1] < 0:
            continue
        tile = [pos[0] + i[0], pos[1] + i[1]]
        print("Manipulator at " + str(tile))
        if unpainted(tile):
            paint(tile)


def paint(pt):
    print("Painting" + str(pt))
    mapList[pt[0]][pt[1]] = "+"
    seen.append(pt)


def unpainted(pt):
    try:
        return mapList[pt[0]][pt[1]] == "-"
    except IndexError as error:
        return False


def valid(pt):
    try:
        return mapList[pt[0]][pt[1]] == "-" or mapList[pt[0]][pt[1]] == "+"
    except IndexError as error:
        return False


def two_turns(dir):
    if dir[2] == 'A':
        return direction[2] == "D"
    if dir[2] == 'D':
        return direction[2] == "A"
    if dir[2] == 'S':
        return direction[2] == "W"
    if dir[2] == 'W':
        return direction[2] == "S"


def move(old, dir):
    print("Current Direction: " + str(dir))
    ahead = [old[0] + dir[0], old[1] + dir[1]]
    if valid(ahead) and ahead[0] >= 0 and ahead[1] >= 0:
        old = ahead
        print("Moving to " + str(ahead))
        print("Move: " + dir[2])
        moves.append(dir[2])
        if two_turns(dir):
            dir = rotate_reverse(dir)
        return ahead, dir

    else:
        dir = rotate(dir)
        moves.append("Q")
        print("Rotating " + str(dir))
        return move(old, dir)


def rotate_reverse(dir):
    if dir[0] == 0 and dir[1] == 1:
        new = [1, 0, 'D']
    elif dir[0] == 1 and dir[1] == 0:
        new = [0, -1, 'S']
    elif dir[0] == 0 and dir[1] == -1:
        new = [-1, 0, 'A']
    else:
        new = [0, 1, 'W']
    return new


def rotate(dir):
    if dir[0] == 0 and dir[1] == 1:
        new = [-1, 0, 'A']
    elif dir[0] == -1 and dir[1] == 0:
        new = [0, -1, 'S']
    elif dir[0] == 0 and dir[1] == -1:
        new = [1, 0, 'D']
    else:
        new = [0, 1, 'W']
    return new


rnd = 0
while len(seen) < seen_count and len(moves) < 80:
    rnd += 1
    print("\nMove " + str(rnd) + ": " + str(len(seen)) + " out of " + str(seen_count) + " seen")
    print("Current: " + str(current))
    if unpainted(current):
        paint(current)
    check_manipulators(current, direction)
    current, direction = move(current, direction)


print("\n")
print("".join([str(x) for x in moves]))
engine.stdin.write(b'{ "cmd": "exit" }\n')
