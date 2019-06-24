#!/usr/bin/env python3
import subprocess
import sys
import json
import random

engine = subprocess.Popen(
    './game_engine/engine.native',
    stdout=subprocess.PIPE,
    stdin=subprocess.PIPE,
    bufsize=0)

maxmoves = 100000
prob = sys.argv[1]
task_json = subprocess.check_output(["./bin/parse-task", "./data/" + prob + ".desc"], universal_newlines=True)
print ("Loading game " + prob)
engine.stdin.write(task_json.encode())
engine.stdin.write(b'\n')
result = engine.stdout.readline()
print(result.decode())

manipulators = {
    "D": [[1, 0], [1, 1], [1, -1]],
    "W": [[0, 1], [-1, 1], [1, 1]],
    "A": [[-1, 0], [-1, -1], [-1, 1]],
    "S": [[0, -1], [1, -1], [-1, -1]],
}
prev_pts = []
dir_pos = {
    'A': [-1, 0],
    'S': [0, -1],
    'W': [0, 1],
    'D': [1, 0]
}
dirs = ['D', 'W', 'A', 'S']


def valid(pt):
    try:
        return mapList[pt[0]][pt[1]] != "W" and mapList[pt[0]][pt[1]] != "O" and pt[0] >= 0 and pt[1] >= 0
    except IndexError as error:
        return False


def is_painted(pt):
    try:
        return mapList[pt[0]][pt[1]] == "+" and pt[0] >= 0 and pt[1] >= 0
    except IndexError as error:
        return False


def is_unpainted(pt):
    try:
        return mapList[pt[0]][pt[1]] == "-" and pt[0] >= 0 and pt[1] >= 0
    except IndexError as error:
        return False


def find_adjacent_unpainted(pos):
    possible = []
    for i in dirs:
        ahead = [pos[0] + dir_pos[i][0], pos[1] + dir_pos[i][1]]
        #print(str(ahead))
        #print(is_unpainted(ahead))
        if is_unpainted(ahead):
            possible.append(i)
    print("Adjacent Unpainted: " + str(possible))
    return possible


def find_possible_moves(pos):
    possible = []
    for i in dirs:
        ahead = [pos[0] + dir_pos[i][0], pos[1] + dir_pos[i][1]]
        if valid(ahead):
            possible.append(i)
    return possible


queued_moves = []
queued_pt = [0, 0]


def move(pos, dir):

    global queued_pt
    adjacent_unpainted = find_adjacent_unpainted(pos)
    if len(adjacent_unpainted) > 0:
        if ((adjacent_unpainted[0] == "W" and dir == 'D') or
                (adjacent_unpainted[0] == "D" and dir == 'S') or
                (adjacent_unpainted[0] == "S" and dir == 'A') or
                (adjacent_unpainted[0] == "A" and dir == 'W')):
            rotate = 'Q'
        else:
            rotate = 'E'

        #print("Rotating " + rotate)
        return rotate
    if len(queued_moves) > 0 and queued_pt in unwrapped_cells:
        print(str(len(queued_moves)) + " Moves left in queue")
        print("Processing From Queue en route to " + str(queued_pt))
        return queued_moves.pop(0)
    queued_moves.clear()
    # lets look for somewhere to go
    nearest = unwrapped_cells[0]
    nearest_diff = abs(pos[0] - nearest[0]) + abs(pos[1] - nearest[1])
    for i in unwrapped_cells:
        #print("Comparing " + str(pos) + " with " + str(i))
        diff = abs(pos[0] - i[0]) + abs(pos[1] - i[1])
        if diff < nearest_diff:
            nearest = i
            nearest_diff = diff

    engine.stdin.write(
        ('{ "cmd": "get_path", "target": [' + str(nearest[0]) + ',' + str(nearest[1]) + '] }\n').encode())
    res = engine.stdout.readline()
    d = json.loads(res.decode())
    queued_pt = nearest
    print("Nearest: " + str(nearest) + " - " + str(nearest_diff) + " Distance: " + str(len(d["path_commands"])))
    for m in d["path_commands"][0:random.randrange(15) + 1]:
        queued_moves.append(m)

    #print("Queued up: " + str(queued_moves))
    print("".join([str(x) for x in queued_moves]))

    return queued_moves.pop(0)
    #return move_random(pos)


def move_random(pos):
    possible = find_possible_moves(pos)
    print("Possible Moves: " + str(possible))
    choice_index = random.randrange(len(possible))
    m = possible[choice_index]
    moves.append(m)
    pt = [pos[0] + dir_pos[m][0], pos[1] + dir_pos[m][1]]
    print("Moving to " + str(pt))
    print("Move: " + possible[choice_index])
    return m


def rotate_reverse(dir):
    if dir == 'W':
        return 'A'
    elif dir == 'A':
        return 'S'
    elif dir == 'S':
        return 'D'
    else:
        return 'W'


def rotate(dir):
    if dir == 'W':
        return 'D'
    elif dir == 'D':
        return 'S'
    elif dir == 'S':
        return 'A'
    else:
        return 'W'


rnd = 0
states = []

direction = 'D'
state_string = ''
current = [0,0]
mapList = []
unwrapped_cells = []
moves = []
orientation = '>'
inventory = []
boosters = []
orientation_map = {">": 'D', "<": 'A', "^": 'W', "v": "S"}

def unpack_state(data):
    global mapList, unwrapped_cells, state_string, current, orientation, boosters, inventory
    mapList = data["map"]
    unwrapped_cells = data["unwrapped_cells"]
    #state_string = data["state_string"]
    current = data["bot_position"]
    orientation = data["workers"][0]["orientation"]
    boosters = data["boosters"]
    inventory = data["inventory"]

next_manip_pos = [
    [1,2],
    [1,-2],
    [0,2],
    [0,-2],
    [-1,2],
    [-1,-2],
    [-1,1],
    [-1,-1],
    [-1,0],
]
engine.stdin.write(b'{ "cmd": "get_state" }\n')
result = engine.stdout.readline()
print(result.decode())
data = json.loads(result.decode())
unpack_state(data)
#print(data['state_string'])

print("there are " + str(len(unwrapped_cells)) + " tiles to paint\n")
while len(moves) < maxmoves:

    if 'B' in inventory and len(next_manip_pos) > 0:
        coords = next_manip_pos.pop(0)
        moves.append('B(' + ','.join(map(str, coords)) + ')')
        engine.stdin.write(('{ "cmd": "action", "action": "B(' + ','.join(map(str, coords)) + ')" }\n').encode())
        engine.stdout.readline()

    rnd += 1
    print("\nMove " + str(len(moves) + 1) + ": " + str(len(unwrapped_cells)) + " left")
    print("Current: " + str(current) + " Facing " + orientation_map[orientation])

    action = move(current, orientation_map[orientation])
    moves.append(action)
    print("Sending Move: " + action)
    engine.stdin.write(('{ "cmd": "action", "action": "' + str(action) + '" }\n').encode())

    result = engine.stdout.readline()
    data = json.loads(result.decode())
    #print(data['state_string'])
    unpack_state(data)

    if data['status'] != 'OK':
        print(data['status'])
        break


print("\n")
print("".join([str(x) for x in moves]))
engine.stdin.write(b'{ "cmd": "exit" }\n')



