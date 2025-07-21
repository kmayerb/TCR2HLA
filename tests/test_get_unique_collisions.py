import unittest
import pytest
from tcrtest.collision import get_unique_collisions_one_cpu, get_unique_collisions, collision


class TestGetCollisions(unittest.TestCase):


    def test_get_unique_collisions_one_cpu(self):
        result = get_unique_collisions_one_cpu(["abc","abb"], collision_func = collision)
        expected = ['.bc',
					 'a.c',
					 'ab.',
					 'a.bc',
					 'ab.c',
					 'abc.',
					 '.bb',
					 'a.b',
					 'ab.',
					 'a.bb',
					 'ab.b',
					 'abb.']
        self.assertEqual(sorted(result), sorted(expected), "collision function failed for 'abc'")

    # For some reason this is failing wiht different numbers of cpus
    def test_get_unique_collisions(self):
        result = get_unique_collisions(["abc","abb"],2, collision_func = collision)
        expected = ['.bc',
					 'a.c',
					 'ab.',
					 'a.bc',
					 'ab.c',
					 'abc.',
					 '.bb',
					 'a.b',
					 'ab.',
					 'a.bb',
					 'ab.b',
					 'abb.']
        self.assertEqual(sorted(result), sorted(expected), "get_unique_collisions function failed for 'abcd'")


if __name__ == '__main__':
    unittest.main()