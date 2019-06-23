#!/usr/bin/env python

import sys
sys.path.append('lib/')
import json
import numpy as np
import subprocess
import random

def get_closest_loc(cur_loc, locs):
    closest_loc = locs.pop(0)

    for loc in locs:
        if abs(cur_loc[0] - loc[0]) + abs(cur_loc[1] - loc[1]) < abs(cur_loc[0] - closest_loc[0]) + abs(cur_loc[1] - closest_loc[1]):
            closest_loc = loc


    return closest_loc

mv_cmds = {
    'pos_y': 'W',
    'neg_y': 'S',
    'pos_x': 'D',
    'neg_x': 'A',
}

def get_next_move(cur_loc, next_loc):
    # x diff is greater than y diff
    if abs(cur_loc[0] - next_loc[0]) > abs(cur_loc[1] - next_loc[1]):
        if cur_loc[0] < next_loc[0]:
            move = mv_cmds["pos_x"]
        else :
            move = mv_cmds["neg_x"]
    else :
        if cur_loc[1] < next_loc[1]:
            move = mv_cmds["pos_y"]
        else :
            move = mv_cmds["neg_y"]
    return move

if __name__ == "__main__":
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
    old_uw_cnt = len(unwrapped)
    new_uw_cnt = 0
    dirs = ['W', 'S', 'D', 'A']
    cur_loc = data["bot_position"]
    status = data["status"]
    final_moves = []

    np_map = np.array(map_list)
    #np.set_printoptions(threshold=sys.maxsize, linewidth=1000)
    print("Shape: " + str(np_map.shape))
    #print(np_map)
    #print("\n\nunwrapped:")
    #print(unwrapped)

    cnt = 0
    stop_rnd_cnt = 10
    use_longest = True
    while len(unwrapped) != 0:
        if status == 'error: Invalid state' :
            use_longest = not use_longest
#            move = random.choice(dirs)
#            stop_rnd_cnt = 0
#        elif old_uw_cnt == new_uw_cnt and stop_rnd_cnt < 10 :
#            move = random.choice(dirs)
#            stop_rnd_cnt += 1
#        else :
        next_loc = get_closest_loc(cur_loc, unwrapped, use_longest)
        move = get_next_move(cur_loc, next_loc)
        print("Trying " + move)

        engine.stdin.write(('{ "cmd": "action", "action": "' + str(move) + '" }\n').encode())
        result = engine.stdout.readline()
#        engine.stdin.write(b'{ "cmd": "get_path", "location": "(' + str(next_loc[0]) + ',' + str(next_loc[1]) + ')" }\n')
#        result = engine.stdout.readline().decode()
#        for char in result["path"]:
#            engine.stdin.write(b'{ "cmd": "action", "action": "' + char + '" }\n')

        # update values now
        data = json.loads(result.decode())
        status = data["status"]
        if status != 'error: Invalid state' :
            print(move)
            cnt += 1
            final_moves.append(move)
        unwrapped = data["unwrapped_cells"]
        old_uw_cnt = new_uw_cnt
        new_uw_cnt = len(unwrapped)
        print("new unwrapped:")
        print(unwrapped)

    print("\n")
    engine.stdin.write(b'{ "cmd": "exit" }\n')

    print("moves: " + ''.join(final_moves))

