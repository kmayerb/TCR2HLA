import itertools
import os
import pandas as pd
from scipy.sparse import csr_matrix, save_npz, dok_matrix
import sys
import time
import psutil
import numpy as np


def get_multimer_dictionary(random_strings, 
                            indels = True, 
                            conserve_memory = False, 
                            ref_d = None, 
                            trim_left = None,
                            trim_right = None, 
                            verbose = False):
    """
    Generate a dictionary mapping multi-mer sequences to their original sequence 
    indices, optionally including indels.

    This function processes a list of random string sequences (e.g., T-cell receptor sequences), creating a dictionary
    that maps each sequence and its possible one-character mismatches (and optionally, one-character indels) to the indices
    of the original sequences. If `conserve_memory` is enabled and a reference dictionary is provided, it filters
    the resulting dictionary to include only keys that are also found in the reference dictionary.

    Parameters
    ----------
    random_strings : list of str
        The list of string sequences to process.
    indels : bool, optional
        Whether to include one-character insertions and deletions in the mismatches. Default is True.
    conserve_memory : bool, optional
        If True, conserves memory by keeping only the keys that are present in both `d` and `ref_d`. Requires `ref_d`
        to be not None. Default is False.
    ref_d : dict, optional
        Reference dictionary used to filter keys when conserving memory. Must be provided if `conserve_memory` is True.
        Default is None.
    trim_left : int, optional
        Number of characters to remove from the start of each sequence before processing. Default is 2.
    trim_right : int, optional
        Number of characters to remove from the end of each sequence before processing. Default is -2
    Returns
    -------
    dict
        A dictionary where each key is a sequence or a sequence with one mismatch/indel, and the value is a list of
        indices from `random_strings` where the (mis)matched sequence originated.

    Raises
    ------
    AssertionError
        If `conserve_memory` is True but `ref_d` is None.

    Notes
    -----
    The function prints progress and timing information, indicating the number of sequences processed and the total
    processing time. Memory conservation mode prints additional information about memory optimization steps.

    Examples
    --------
    >>> random_strings = ["ABCDEFGH", "ABCGEFGH", "QRSTUVWX"]
    >>> result = get_multimer_dictionary(random_strings, trim_left = 2, trim_right = -2, indels=False)
    >>> expected = {'.DEF': [0],
                     'C.EF': [0, 1],
                     'CD.F': [0],
                     'CDE.': [0],
                     '.GEF': [1],
                     'CG.F': [1],
                     'CGE.': [1],
                     '.TUV': [2],
                     'S.UV': [2],
                     'ST.V': [2],
                     'STU.': [2]}
    >>> assert result == expected
    """
   
    # OPTIONAL TRIMMING
    if trim_left is None and trim_right is not None: 
        if trim_right > 0:
            trim_right = -1*trim_right
    
        if verbose: print(f"Right trimming input sequences by {trim_right} only.")
        random_strings = [x[:trim_right] for x in random_strings]
    
    elif trim_right is None and trim_left is not None:
        if verbose: print(f"Left trimming input sequences by {trim_left} only.")
        random_strings = [x[:trim_right] for x in random_strings]
    
    elif trim_left is None and trim_right is None:
        if verbose: print("No trimming of input sequences performed.")
        pass
    else: 
        if trim_right > 0:
            trim_right = -1*trim_right
        if verbose: print(f"Left trimming input sequences by {trim_left} and rRight trimming by {trim_right}.")
        random_strings = [x[trim_left:trim_right] for x in random_strings]


    tic = time.perf_counter()
    if verbose: print(f"Finding multi-mers from {len(random_strings)} sequences, expect 1 min per million")
    d = dict()
    for i, cdr3 in enumerate(random_strings):
        if isinstance(cdr3, str):
            mm = [cdr3[:i-1] + "." + cdr3[i:] for i in range(1, len(cdr3)+1)]
            if indels:
                indels = [cdr3[:i] + "." + cdr3[i:] for i in range(1, len(cdr3)+1)]
                mm = mm + indels
            for m in mm:
                d.setdefault(m, []).append(i)
        else:
            pass
        if i % 100000 == 0:
            if conserve_memory:
                if ref_d is None:
                    raise ValueError
                if verbose: print(f"\tprocessed {i} TCRs - conserving hash memory by dumping unmatched keys")
                # Drop unneed keys
                assert ref_d is not None
                common_keys = d.keys() & ref_d.keys()
                # Create a new dictionary from dq with only the common keys
                d = {k: d[k] for k in common_keys}
    toc = time.perf_counter()
    if verbose: print(f"\tStored 1 mismatch/indel features in {len(random_strings)/1E6} M sequences in {toc - tic:0.4f} seconds")
    return d

def get_query_v_query_npz_dok(dq, n_rows, n_cols, nn_min = 1, verbose = False):
    """
    Constructs a sparse matrix (CSR format) representing linkages between pairs of indices in `dq` exceeding a minimum count.

    This function iterates over a dictionary `dq` to find all pairs of indices (representing linkages) that occur more
    than `nn_min` times. It then constructs a Compressed Sparse Row (CSR) matrix where each row and column represent an
    index in `dq`, and the value at (row, col) is 1 if a linkage between those indices exists, indicating shared 
    linkages between the subject and query datasets.

    Parameters
    ----------
    dq : dict
        A dictionary where keys are query identifiers and values are lists of indices representing linkages.
    n_rows : int
        The number of rows for the CSR matrix, corresponding to the number of unique indices in `dq`.
    n_cols : int
        The number of columns for the CSR matrix, identical in meaning to `n_rows`.
    nn_min : int, optional
        The minimum number of occurrences for a linkage to be included in the matrix. Default is 1.

    Returns
    -------
    csr_matrix
        A scipy.sparse.csr_matrix object representing the linkages between pairs of indices in `dq`. The matrix has
        shape (n_rows, n_cols) and dtype 'int8', where each non-zero entry indicates a linkage.

    Examples
    --------
    >>> dq = {'key1': [0, 1, 2], 'key2': [2, 3], 'key3': [1, 4]}
    >>> n_rows = 5  # Assuming 5 unique indices
    >>> n_cols = 5  # Same as n_rows
    >>> csr_mat = get_query_v_query_npz(dq, n_rows, n_cols, nn_min=1)
    >>> print(csr_mat.shape)
    (5, 5)
    """
    # Detect all shared linkages between the subject and query
    from scipy.sparse import dok_matrix
    dok = dok_matrix((n_rows, n_cols), dtype=np.dtype('u1'))
    tic = time.perf_counter()
    x = [k for k,v in dq.items() if len(v) > nn_min]
    for i in x:
        ix = dq[i]
        for tup in itertools.permutations(ix, 2):
            dok[tup]=1
    toc = time.perf_counter()
    if verbose:
        print(f"Found self linkages in {toc - tic:0.4f} seconds")
        print(f"\tConverting linkages to <csr_mat>")
    tic = time.perf_counter()
    dok.setdiag(1)
    csr_mat = dok.tocsr()
    toc = time.perf_counter()
    if verbose:
        print(f"\tConstructed sparse matrix in {toc - tic:0.4f} seconds")
    return csr_mat
    
    
def get_query_v_subject_npz_dok(dq, ds, n_rows, n_cols, nn_min = 0, verbose = False):
    # Detect all shared linkages between the subject and query
    # dq : query hash
    # ds : subject hash
    from scipy.sparse import dok_matrix
    # Initialize a dok_matrix
    dok = dok_matrix((n_rows, n_cols), dtype=np.dtype('u1'))
    tic = time.perf_counter()
    # <x> We only care about keys in the query
    x = [k for k,v in dq.items() if len(v) > nn_min]
    for i in x:
        ix = dq.get(i)
        jx = ds.get(i)
        if jx is not None:
            for tup in itertools.product(ix, jx):
                dok[tup] = 1
    csr_mat = dok.tocsr()
    toc = time.perf_counter()
    if verbose:
        print(f"\tConstructed sparse matrix  {toc - tic:0.4f} seconds")
    return csr_mat


def get_query_v_subject_identity_npz_dok(seqs_q, seqs_s, verbose = False):
    from scipy.sparse import dok_matrix
    ds = dict()
    n_rows = len(seqs_q)
    n_cols = len(seqs_s)
    dok = dok_matrix((n_rows, n_cols), dtype=np.dtype('u1'))
    tic = time.perf_counter()
    for i,s in enumerate(seqs_s):
        ds.setdefault(s,[]).append(i)
    for i,q in enumerate(seqs_q):
        if ds.get(q) is not None: 
            jx = ds.get(q)
            for j in jx:
                dok[(i,j)] = 1
    csr_mat = dok.tocsr()
    toc = time.perf_counter()
    if verbose:
        print(f"\tConstructed sparse matrix {toc - tic:0.4f} seconds")
    return csr_mat
    """
    s1 = ['AAA','AAB','ABB','BBB']
    s2 = ['AAA','XXX','XXX','BBB','XXX','AAB','ABB','BBB']
    get_query_v_subject_identity_npz_dok(seqs_q=s1, seqs_s=s2)

    """




def write_csr_mat(csr_mat, output_csrmat):
    tic = time.perf_counter()
    save_npz(output_csrmat, csr_mat)
    toc = time.perf_counter()
    print(f"\tCSR_MAT: {csr_mat.shape[0]}x{csr_mat.shape[1]}")
    print(f"\t\tTotal connections: {csr_mat.sum()}")
    print(f"Wrote sparse matrix in {toc - tic:0.4f} seconds to {output_csrmat}")


