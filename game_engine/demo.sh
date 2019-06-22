#!/bin/sh

(

  ../bin/parse-task ../data/prob-002.desc;
  echo '{"cmd": "print_state"}';
  echo '{"cmd": "do_moves", "moves": "FBLR"}'
  # echo '{"cmd": "get_state"}';
  echo '{"cmd": "exit"}';

) | ./engine.native
