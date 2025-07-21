import unittest
import pandas as pd
import numpy as np
from tcrtest.permutation import yield_permutations, get_identity, get_permutations

class TestPermutationFunctions(unittest.TestCase):
    def setUp(self):
        self.df_single = pd.DataFrame({'h': [1], 'i': [1]})
        self.df_multi = pd.DataFrame({
            'h': [1, 1, 2, 2, 2],
            'i': [1, 2, 3, 4, 5]
        })
        self.df_small= pd.DataFrame({
            'h': [1, 1, 2, 2],
            'i': [1, 2, 3, 4]
        })

    def test_yield_permutations_single(self):
        result = list(yield_permutations(self.df_single))
        self.assertEqual(result, [])

    def test_yield_permutations_multi(self):
        expected = [(1, 2), (2, 1), (3, 4), (3, 5), (4, 3), (4, 5), (5, 3), (5, 4)]
        result = list(yield_permutations(self.df_multi))
        self.assertEqual(result, expected)

    # def test_yield_identity_single(self):
    #     result = list(yield_identity(self.df_single))
    #     self.assertEqual(result, [(0, 0)])

    # def test_yield_identity_multi(self):
    #     expected = [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)]
    #     result = list(yield_identity(self.df_multi))
    #     self.assertEqual(result, expected)

    def test_get_identity_multi_array(self):
        result = get_identity(5, as_df=False)
        print(result)
        expected = (np.array([0, 1, 2, 3, 4], dtype=int), 
                    np.array([0, 1, 2, 3, 4], dtype=int))
        assert np.all(result[0] == expected[0])
        assert np.all(result[1] == expected[1])
        
    def test_get_permutations_multi_array(self):
        result = get_permutations(self.df_small, as_df=False)
        expected = (np.array([1, 2, 3, 4,], dtype=int), 
                    np.array([2, 1, 4, 3], dtype=int))
        assert np.all(result[0] == expected[0])
        assert np.all(result[1] == expected[1])

if __name__ == '__main__':
    unittest.main()