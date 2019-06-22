import unittest
import sys
sys.path.append('bots/')
from nearest_space import get_closest_loc

def test_closet():
    locs = [[1,2], [3,4], [5,6], [7,8]]
    cur_loc = [0,0]
    got_loc = get_closest_loc(cur_loc, locs)
    assert got_loc == [1,2]

    cur_loc = [3,3]
    locs = [[4,4], [0,0], [2,2], [3,5], [2,1]]
    got_loc = get_closest_loc(cur_loc, locs)
    assert got_loc == [4,4]
