from tcrtest.similar import get_multimer_dictionary, get_query_v_query_npz_dok, get_query_v_subject_npz_dok 
import numpy as np 

def test_similar_basic_square():
    import sys
    from scipy.sparse import csr_matrix
    folder_path = '/fh/fast/gilbert_p/fg_data/fast_edit1/'
    sys.path.append(folder_path)
    
    seqs = ['AAAA','AAAA','ABAA','ACAA','CAAA','CAAA','XXXX','YYYY']
    dq   = get_multimer_dictionary(seqs, trim_left =None, trim_right = None)
    s1   = get_query_v_query_npz_dok(dq, len(seqs), len(seqs), nn_min = 0) 
    assert isinstance(s1, csr_matrix) 
    expected = np.array([[1,1,1,1,1,1,0,0],
                         [1,1,1,1,1,1,0,0],
                         [1,1,1,1,0,0,0,0],
                         [1,1,1,1,0,0,0,0],
                         [1,1,0,0,1,1,0,0],
                         [1,1,0,0,1,1,0,0],
                         [0,0,0,0,0,0,1,0],
                         [0,0,0,0,0,0,0,1]])
    assert np.all(s1.todense() == expected)

def test_similar_basic_rect():
    import sys
    from scipy.sparse import csr_matrix
    folder_path = '/fh/fast/gilbert_p/fg_data/fast_edit1/'
    sys.path.append(folder_path)
    
    seqs = ['AAAA','AAAA','ABAA','ACAA','CAAA','CAAA','XXXX','YYYY']
    seqs2 = ['AAAA','AAAA','ABAA','ACAA','CAAA','CAAA','XXXX','YYYY',
             'AAAA','AAAA','ABAA','ACAA','CAAA','CAAA','XXXX','YYYY']
    dq   = get_multimer_dictionary(seqs, trim_left =None, trim_right = None)
    ds   = get_multimer_dictionary(seqs2, trim_left =None, trim_right = None)
    s1   = get_query_v_subject_npz_dok(dq, ds, len(seqs), len(seqs2), nn_min = 0) 
    assert isinstance(s1, csr_matrix) 
    expected = np.array([[1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0],
                         [1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0],
                         [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0],
                         [1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0],
                         [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0],
                         [1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0],
                         [0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0],
                         [0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1]])
    assert np.all(s1.todense() == expected)