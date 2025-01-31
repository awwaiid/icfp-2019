#!/usr/bin/env python

import sys
sys.path.append('lib/')
import json
import numpy as np
import subprocess
import random
random.seed(42)

engine = subprocess.Popen(
    './game_engine/engine.native',
    stdout=subprocess.PIPE,
    stdin=subprocess.PIPE,
    bufsize=0)

problem = 'prob-001.desc'
if len(sys.argv) > 1: problem = sys.argv[1]
task_json = subprocess.check_output(["./bin/parse-task", f'./data/{problem}'], universal_newlines=True)

engine.stdin.write(task_json.encode())
engine.stdin.write(b'\n')
result = engine.stdout.readline()
print(result.decode())

engine.stdin.write(b'{ "cmd": "get_state" }\n')
result = engine.stdout.readline()
print(result.decode())

data = json.loads(result.decode())
map_list = data["map"]
unwrapped = data["unwrapped_cells"]
direction = [1, 0, 'D']
cur_loc = data["bot_position"]
final_moves = []


np_map = np.array(map_list)
np.set_printoptions(threshold=sys.maxsize, linewidth=1000)
print("Shape: " + str(np_map.shape))
print(np_map)
print("\n\nunwrapped:")
print(unwrapped)

cnt = 0
while len(unwrapped) != 0 :
    next_loc = random.choice(unwrapped)
    #print('{ "cmd": "get_path", "target": [' + str(next_loc[0]) + ',' + str(next_loc[1]) + '] }\n')
    engine.stdin.write(('{ "cmd": "get_path", "target": [' + str(next_loc[0]) + ',' + str(next_loc[1]) + '] }\n').encode())
    result = engine.stdout.readline()
    data = json.loads(result.decode())
#    print(data)
    for move in data["path_commands"]:
        final_moves.append(move)
        engine.stdin.write(('{ "cmd": "action", "action": "' + str(move) + '" }\n').encode())
        result = engine.stdout.readline()
        data = json.loads(result.decode())
        if data["status"] == 'error: Invalid state' : print("####### ERROR: invalid state ######")
        #print(data)

    # update values now
    unwrapped = data["unwrapped_cells"]
    print(unwrapped + "\n\n")
    cnt += 1

print("\n")
engine.stdin.write(b'{ "cmd": "exit" }\n')

print("Moves: " + ''.join(final_moves))

