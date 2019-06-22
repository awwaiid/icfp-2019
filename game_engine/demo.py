#!/usr/bin/env python

import subprocess

engine = subprocess.Popen(
  './engine.native',
  stdout=subprocess.PIPE,
  stdin=subprocess.PIPE,
  bufsize=0)

task_json = subprocess.check_output(["../bin/parse-task","../data/prob-002.desc"],universal_newlines=True)

engine.stdin.write(task_json.encode())
engine.stdin.write(b'\n')
result = engine.stdout.readline()
print(result)

engine.stdin.write(b'{ "cmd": "get_state" }\n')
result = engine.stdout.readline()
print(result.decode())

engine.stdin.write(b'{ "cmd": "exit" }\n')
