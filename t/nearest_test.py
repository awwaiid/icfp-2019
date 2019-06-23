import unittest
import sys
sys.path.append('bots/')
from nearest_space import get_closest_loc, get_next_move

def test_closet():
    locs = [[1,2], [3,4], [5,6], [7,8]]
    cur_loc = [0,0]
    got_loc = get_closest_loc(cur_loc, locs)
    assert got_loc == [1,2]

    cur_loc = [3,3]
    locs = [[4,4], [0,0], [2,2], [3,5], [2,1]]
    got_loc = get_closest_loc(cur_loc, locs)
    assert got_loc == [4,4]

def get_next_move():
    cur_loc = [3,4]
    next_loc = [7,23]
    mv = get_next_move(cur_loc, next_loc)
    assert mv == 'D'

    cur_loc = [3,4]
    next_loc = [0,0]
    mv = get_next_move(cur_loc, next_loc)
    assert mv == 'A'

    cur_loc = [3,4]
    next_loc = [10,4]
    mv = get_next_move(cur_loc, next_loc)
    assert mv == 'W'

    cur_loc = [3,4]
    next_loc = [2,4]
    mv = get_next_move(cur_loc, next_loc)
    assert mv == 'S'
