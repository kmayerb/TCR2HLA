import unittest
import pandas as pd
import numpy as np
from tcrtest.testing import matrix_tab_w_missing_info, matrix_tab

class TestMatrixTabWithMissing(unittest.TestCase):
    def test_convert_format(self):
        p2 = np.array([
            np.vstack([1, 0, 1]),
            np.vstack([0, 1, 1])])
        # CONVERT
        if p2.ndim == 3 and p2.shape[2] == 1:
            p1 = np.array([np.squeeze(arr) for arr in p2])
        p = np.array([
                [1, 0, 1],
                [0, 1, 1]
        ])
        assert np.all(p1 == p)
        print("Presqueeze")
        print(p2.shape)
        print(p1.shape)


    def test_basic_functionality1(self):
        # COMPARE TO BASIC FUNCTIONALITY OF ORIGINAL MATRIX_TAB


        # Test with standard inputs without NaNs
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [1, 0],
            [0, 1],
            [1, 1]
        ])

        expected_output = pd.DataFrame(
            {'a': [2, 1, 1, 2], 
             'b': [0, 1, 1, 0], 
             'c': [0, 1, 1, 0],
             'd': [1, 0, 0, 1],
             'i': [0, 0, 1, 1],
             'm': [0, 1, 0, 1]})
        result = matrix_tab_w_missing_info(M=M, p=p)
        
        print("New result")
        print(result)
        p2 = np.array([
            np.vstack([1, 0, 1]),
            np.vstack([0, 1, 1])])
        print("Classic result")
        #print(p2.shape)
        result_classic = matrix_tab(M=M ,p=p2, idx = range(0,p2.shape[0]))
        print(result_classic)
        print(result_classic.to_dict('list'))

        result3 = matrix_tab_w_missing_info(M=M, p=p2)
        # GIVEN ORIGINAL THE WRONT FORMAT p
        result4 = matrix_tab(M=M, p=p, idx = range(0,p2.shape[0]))
        
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), result_classic.reset_index(drop = True).astype('uint32'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))
        pd.testing.assert_frame_equal(result3.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))
        pd.testing.assert_frame_equal(result4.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))
    
    
    def test_with_nan_values(self):
        # Test with NaN values in M
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [np.nan, 0],
            [0, 1],
            [1, 1]
        ])
        expected_output = pd.DataFrame(
            {'a':[1, 1, 1, 2], 
            'b': [0, 1, 1, 0],
            'c': [0, 1, 0, 0],
            'd': [1, 0, 0, 1],
            'i': [0, 0, 1, 1],
            'm': [0, 1, 0, 1]}
            )
        
        result = matrix_tab_w_missing_info(M=M, p=p)
        print("1 np.nan, first column")
        print(result)
        #print(result.to_dict('list'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))

    
    def test_with_None_values(self):
        # Test with NaN values in M
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [None, 0],
            [0, 1],
            [1, 1]
        ])
        expected_output = pd.DataFrame(
            {'a':[1, 1, 1, 2], 
            'b': [0, 1, 1, 0],
            'c': [0, 1, 0, 0],
            'd': [1, 0, 0, 1],
            'i': [0, 0, 1, 1],
            'm': [0, 1, 0, 1]}
            )
        
        result = matrix_tab_w_missing_info(M=M, p=p)
        #print(result)
        #print(result.to_dict('list'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))

    def test_with_all_None_values(self):
        # Test with NaN values in M
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [None, 0],
            [None, 1],
            [None, 1]
        ])
        expected_output = pd.DataFrame(
            {'a':[0, 1, 0, 2], 
            'b': [0, 1, 0, 0],
            'c': [0, 1, 0, 0],
            'd': [0, 0, 0, 1],
            'i': [0, 0, 1, 1],
            'm': [0, 1, 0, 1]}
            )
        
        result = matrix_tab_w_missing_info(M=M, p=p)
        print("ALL NONE, 1 COL")
        print(result)
        #print(result.to_dict('list'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))


    def test_with_all_None_both_columns(self):
        # Test with NaN values in M
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [None, None],
            [None, None],
            [None, None]
        ])
        expected_output = pd.DataFrame(
            {'a':[0, 0, 0, 0], 
            'b': [0, 0, 0, 0],
            'c': [0, 0, 0, 0],
            'd': [0, 0, 0, 0],
            'i': [0, 0, 1, 1],
            'm': [0, 1, 0, 1]}
            )
        
        result = matrix_tab_w_missing_info(M=M, p=p)
        print("ALL NONE, BOTH COL")
        print(result)
        #print(result.to_dict('list'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))



    def test_with_subset_column(self):

        # Test with standard inputs without NaNs
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [1, 0],
            [0, 1],
            [1, 1]
        ])
        result = matrix_tab_w_missing_info(M=M, p=p, subset_column=0)
        print("Subset_Column 0")
        print(result)

        expected_output = pd.DataFrame(
            {'a': [2, 1, 1, 1], 
             'b': [0, 1, 0, 0], 
             'c': [0, 0, 1, 0], 
             'd': [0, 0, 0, 1],
             'i': [0, 0, 1, 1], 
             'm': [0, 1, 0, 1]})
        print(result.to_dict('list'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))

        #pd.testing.assert_frame_equal(result.reset_index(drop=True), expected_output)
    
    def test_with_conditional_column(self):
        # Test with conditional_column specified
        # Test with standard inputs without NaNs
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [1, 0],
            [0, 1],
            [1, 1]
        ])
        result = matrix_tab_w_missing_info(M=M, p=p, subset_column=0)
        print("Conditional_Column 0")
        print(result)

        expected_output = pd.DataFrame(
            {'a': [2, 1, 1, 1], 
             'b': [0, 1, 0, 0], 
             'c': [0, 0, 1, 0], 
             'd': [0, 0, 0, 1],
             'i': [0, 0, 1, 1], 
             'm': [0, 1, 0, 1]})
        print(result.to_dict('list'))
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.reset_index(drop = True).astype('uint32'))
    
    def test_empty_inputs(self):
        # Test with empty inputs
        p = np.array([[], []])
        M = np.array([[]])
        expected_output = pd.DataFrame({
            'a': [],
            'b': [],
            'c': [],
            'd': [],
            'i': [],
            'm': []
        })
        result = matrix_tab_w_missing_info(M=M, p=p)
        pd.testing.assert_frame_equal(result.reset_index(drop=True).astype('uint32'), expected_output.astype('uint32') )
    

    def test_with_idx_parameter(self):
        # Test with idx parameter specified
        p = np.array([
            [1, 0, 1],
            [0, 1, 1]
        ])
        M = np.array([
            [1, 0],
            [0, 1],
            [1, 1]
        ])
        idx = ['biomarker_A', 'biomarker_B']
 
        expected_output = pd.DataFrame(
            {'a': [2, 1, 1, 2], 
             'b': [0, 1, 1, 0], 
             'c': [0, 1, 1, 0],
             'd': [1, 0, 0, 1],
             'i': ['biomarker_A', 'biomarker_A', 'biomarker_B', 'biomarker_B'],
             'm': [0, 1, 0, 1]})

        result = matrix_tab_w_missing_info(M=M, p=p, idx=idx)
        assert np.all(result['i'] == expected_output['i'])


if __name__ == '__main__':
    unittest.main()
