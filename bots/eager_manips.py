#!/usr/bin/env python

import sys
sys.path.append('lib/')
import json
import numpy as np
import subprocess
import random

def manhattan_distance(loc_from, loc_to):
    # print("manhattan_distance", loc_from, loc_to)
    return abs(loc_from[0] - loc_to[0]) + abs(loc_from[1] - loc_to[1])

def sort_by_distance_from(locs, loc_from):
    sort(locs, lambda loc: manhattan_distance(loc_from, loc))

def engine_command_raw(engine, command_data):
    engine.stdin.write(command_data.encode())
    result = engine.stdout.readline().decode()
    data = json.loads(result)
    return data

def engine_command_exit(engine):
    engine.stdin.write(b'{ "cmd": "exit" }\n')
    # engine.stdin.write(json.dumps({"cmd", "exit"}).encode())
    # engine.stdin.write(b"\n")

def engine_command(engine, command_data):
    engine.stdin.write(json.dumps(command_data).encode())
    result = engine.stdout.readline().decode()
    data = json.loads(result)
    return data

def engine_command_add_manipulator(engine, coords):
    final_moves.append('B(' + ','.join(map(str, coords)) + ')')
    return engine_command(engine, { "cmd": "action", "action": f"B({','.join(map(str, coords))})"})

def get_path_commands(engine, target):
    data = engine_command(engine, { 'cmd': 'get_path', 'target': target })
    return data["path_commands"]

def get_closest_loc(cur_loc, locs):
    # print("get_closest_loc", cur_loc, locs)
    closest_loc = locs.pop(0)
    for loc in locs:
        # print(f"Looking at distance for loc {loc} to see if it is less than {closest_loc}")
        if manhattan_distance(cur_loc, loc) < manhattan_distance(cur_loc, closest_loc):
            closest_loc = loc

    return closest_loc

def create_engine():
    return subprocess.Popen(
        './game_engine/engine.native',
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE,
        bufsize=0)

if __name__ == "__main__":
    engine = create_engine()

    problem = 'prob-001.desc'
    if len(sys.argv) > 1: problem = sys.argv[1]

    # Initialize the engine with the task json. Ignore the results
    task_json = subprocess.check_output(
            ["./bin/parse-task", f'./data/{problem}'],
            universal_newlines=True)
    engine_command_raw(engine, task_json)

    data = engine_command(engine, { "cmd": "get_state" })
    unwrapped = data["unwrapped_cells"]
    cur_loc = data["bot_position"]
    final_moves = []
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

    while len(unwrapped) != 0:
        next_loc = get_closest_loc(cur_loc, unwrapped)
        path_commands = get_path_commands(engine, next_loc)
        for move in path_commands:
            final_moves.append(move)
            data = engine_command(engine, { "cmd": "action", "action": move})
            if data["status"] == 'error: Invalid state' : print("####### ERROR: invalid state ######")
            if 'B' in data["inventory"] and len(next_manip_pos) > 0:
                # attach manipulator, ignore result
                coords = next_manip_pos.pop(0)
                engine_command_add_manipulator(engine, coords)

        # update values now
        unwrapped = data["unwrapped_cells"]
        print(len(unwrapped), file=sys.stderr)
        cur_loc = data["workers"][0]["position"]

    print(''.join(final_moves))
    engine_command_exit(engine)


