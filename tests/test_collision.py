# test_collision.py
import unittest
import pytest
from tcrtest.collision import collision, collision2, kmer_collision, cdr3_5mers_dis13_collision, cdr3_5mers_dis2_collision, deletion
from tcrtest.collision import get_keys_from_seqs_ , get_unique_collisions
import pandas as pd 
from collections import Counter

class TestCollision(unittest.TestCase):
    def setUp(self):
        self.seqs_u = pd.read_csv('dash.csv')['cdr3_b_aa'].unique().tolist()
        self.seqs = pd.read_csv('dash.csv')['cdr3_b_aa'].to_list()

    def test_collision(self):
        result = collision("abc")
        expected = ['.bc', 'a.c', 'ab.', 'a.bc', 'ab.c', 'abc.']
        print(result)
        self.assertEqual(sorted(result), sorted(expected), "collision function failed for 'abc'")

    def test_collision2(self):
        result = collision2("abcd")
        expected = ['..cd', '.b.d', '.bc.', 'a..d', 'a.c.', 'ab..']
        self.assertEqual(sorted(result), sorted(expected), "collision2 function failed for 'abcd'")

    def test_kmer_collision(self):
        result = set(kmer_collision("abcdef", k=3))
        print(result)

        expected = {'bcd', 'abc', 'cde'}
        self.assertEqual(result, expected, "kmer_collision function failed for 'abcdef'")

    def test_cdr3_5mers_dis13_collision(self):
        result = cdr3_5mers_dis13_collision("abcdef")
        expected = ['a.c.e', 'b.d.f']
        self.assertEqual(result, expected, "cdr3_5mers_dis13_collision function failed for 'abcdef'")

    def test_cdr3_5mers_dis2_collision(self):
        result = cdr3_5mers_dis2_collision("abcdefg")
        expected = ['ab.de', 'bc.ef']
        self.assertEqual(result, expected, "cdr3_5mers_dis2_collision function failed for 'abcdefg'")

    def test_deletion(self):
        result = deletion("abc")
        expected = ['bc', 'abc','ac', 'ab']
        self.assertEqual(sorted(result), sorted(expected), "deletion function failed for 'abc'")

    def test_get_keys_from_seqs_(self):
         keys1 = get_keys_from_seqs_(self.seqs, collision_func =collision)
         keys2 = get_keys_from_seqs_(self.seqs_u, collision_func =collision)
         self.assertEqual(set(keys1), set(keys2))

    def test_standard_v_parrallel(self):
        keys2 = get_keys_from_seqs_(self.seqs_u, collision_func =collision)
        for i in [1,2,3,4]:
            print(f"testing with {i} cpus")
            keys2p = get_unique_collisions(self.seqs_u, cpus = i, collision_func = collision)
            self.assertEqual(Counter(keys2), Counter(keys2p) )







if __name__ == '__main__':
    unittest.main()