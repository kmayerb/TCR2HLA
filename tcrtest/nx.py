import sys
folder_path = '/fh/fast/gilbert_p/kmayerbl/tcrtest/'
sys.path.append(folder_path)

import argparse
import os
from datetime import datetime
import numpy as np 
import pandas as pd
from scipy.sparse import csr_matrix, save_npz
import time
from tcrtest.permutation import get_permutations, get_identity
from tcrtest.testing import split_into_chunks
from tcrtest.collision import collision, get_unique_collisions, get_unique_collisions_one_cpu
from collections import Counter
import psutil


def clock():
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    return current_time

def get_memory_usage():
    process = psutil.Process()  # Get the current process
    mem_info = process.memory_info()  # Get memory info
    return mem_info.rss  # Return the Resident Set Size (RSS) in bytes


def break_range_into_parts(N, k = 10):
    assert k < N
    parts = []
    step = N // k
    for i in range(0, N, step):
        part_start = i
        part_end = min(i + step, N)
        parts.append(np.array(list(range(part_start, part_end)), dtype = np.uint32))
    return parts

def expand_a_csr(S, seqs_u_inv, k =10):
    from scipy.sparse import vstack
    u_inv_parts = break_range_into_parts(N = seqs_u_inv.shape[0], k =k)
    from scipy.sparse import hstack,vstack
    frags = list()
    for part in u_inv_parts:
        frags.append(S[seqs_u_inv[part],:][:,seqs_u_inv])
    S_rexp = vstack(frags)
    del(frags)
    return S_rexp


def map_key_ix_to_seq_ix(ukey:dict,seqs:list,collision_func = collision):
    """
    Maps indices from a dictionary of unique keys to sequence indices based on 
    the output of a collision function applied to each sequence.

    This function iterates over a list of sequences, applies a collision function to each sequence,
    and checks the output against a dictionary of unique keys (`ukey`). If a key from the collision
    function output exists in `ukey`, the function maps the `ukey` index of this key to the sequence index.

    Parameters:
    - ukey (dict): A dictionary with unique keys as keys and their indices as values.
    - seqs (list): A list of sequences on which the collision function will be applied.
    - collision_func (callable): A function that takes a sequence as input and returns an iterable of keys.

    Returns:
    - numpy.ndarray: An array of pairs, where each pair is [key index, sequence index].
                     This array is of dtype `np.uint32`.
    """
    key_ix_seq_ix = list()
    for i,s in enumerate(seqs):
        for x in collision_func(s):
            if ukey.get(x) is not None:
                key_ix_seq_ix.append((ukey.get(x),i))
    key_ix_seq_ix = np.array(key_ix_seq_ix, dtype=np.uint32)
    return(key_ix_seq_ix)
    

def seqs_to_ix(seqs:list, cpus:int, min_occur = 1, collision = collision):
    """
    Processes a list of sequences to identify and index unique sequences based on
    specified collision handling. Reduces the sequence set to unique elements, finds
    degenerate keys, and assigns numeric indices to these keys.

    Steps:
    1. Reduces the input list of sequences to unique sequences.
    2. Identifies unique 'collision' keys in the sequences using multiple CPUs.
    3. Filters these keys based on their occurrence frequency, retaining only those
       that occur more than a specified minimum.
    4. Maps each unique sequence to its respective degenerate key.
    5. Returns a DataFrame mapping keys to sequences, along with additional metadata.

    Parameters:
    - seqs (list): A list of sequences to process.
    - cpus (int): The number of CPUs to use for parallel processing.
    - min_occur (int, optional): The minimum occurrence threshold for a key to be retained. Defaults to 1.

    Returns:
    - tuple: Contains the following elements:
        1. DataFrame with columns ['h', 'i'] where 'h' is the degenerate key and 'i' is the index
           of the unique sequence.
        2. np.ndarray of unique sequences.
        3. np.ndarray of indices that can be used to reconstruct the original list of sequences from the unique ones.
        4. int representing the number of rows in the unique sequence array.
        5. int representing the number of columns in the unique sequence array, typically equal to the number of rows.

    Example:
    >>> seqs = ["AAC", "AAC", "TAG", "CAG"]
    >>> x, seqs_u, seqs_u_inv, n_row_u, n_col_u = seqs_to_ix(seqs, cpus=4)
    >>> print(x)
    >>> print(seqs_u)
    >>> print(seqs_u_inv)
    >>> print(n_row_u, n_col_u
    """
    print(f"{clock()} -- Reducing the input sequence set to a unique sequence set")
    seqs_u, seqs_u_inv = np.unique(seqs, return_inverse=True)

    print(f"{clock()} -- Find all possible collision keys")
    unique_keys = get_unique_collisions(
         seqs=seqs_u,
         cpus=cpus,
         collision_func = collision)
     
    #unique_keys = get_unique_collisions_one_cpu(seqs = seqs_u, collision_func = collision)

    #import pdb; pdb.set_trace()
    print(f"{clock()} -- Keep collision keys that occur more than {min_occur} time")
    unique_keys = Counter(unique_keys)
    collision_dict = dict()
    ix = 0
    for k,v in unique_keys.items():
        if v > min_occur :
            collision_dict[k] = ix
            ix +=1
    #import pdb; pdb.set_trace()
    del(unique_keys)
    

    print(f"{clock()} -- Assign degenerate key 'h' to each unique sequence 'i'")
    x = map_key_ix_to_seq_ix(
        ukey = collision_dict, 
        seqs = seqs_u, 
        collision_func = collision)
    if len(x) == 0: 
        raise ValueError('none of the sequences collided')
        #import pdb; pdb.set_trace()
        #TODO: FIGURE OUT HOW TO DEAL WITH THIS RARE CASE
    x = pd.DataFrame(x , columns = ['h','i'], dtype = 'uint32')
    x = x.sort_values(['h','i']).reset_index(drop = True)

    n_row_u = len(seqs_u)
    n_col_u = len(seqs_u)
    return (x, seqs_u, seqs_u_inv,  n_row_u,  n_col_u )


def seqs_to_csr(seqs, 
    cpus, 
    use_dask = False, 
    memory_limit = '20GB', 
    dask_report_name="dask_report.html",
    return_raw = False,
    min_occur = 1,
    min_nn = None,
    collision = collision):
    """
    Converts a sequence of data into a compressed sparse row (CSR) matrix format, optionally using Dask
    for distributed computing to handle large datasets efficiently.

    This function processes a list of sequences, computes unique occurrences, and then generates a CSR matrix
    from these sequences. The computation can be performed either using local processing or distributed processing
    with Dask.

    Parameters:
    - seqs (list): The list of sequences to process.
    - cpus (int): The number of CPU cores to use for processing.
    - use_dask (bool, optional): Flag to determine whether to use Dask for distributed computing. Defaults to False.
    - memory_limit (str, optional): The memory limit for each Dask worker when Dask is used. Defaults to '20GB'.
    - dask_report_name (str, optional): The name of the file to write the Dask performance report to. Defaults to "dask_report.html".

    Returns:
    If return_raw is False (default):
    - csr_matrix: CSR matrix with 'edit0' modifications, reflecting identity permutations.
    - csr_matrix: CSR matrix with 'edit1' modifications, reflecting all permutations.

    If return_raw is True: (ADVANCED USER ONLY -- SEE WARNINGS)
    - pd.DataFrame: Dataframe of concatenated identity and permutation indices.
    - pd.DataFrame: Dataframe of identity indices only.
    - np.ndarray: Array of unique sequences.
    - np.ndarray: Indices to reconstruct original sequences from unique sequences.
    - int: Number of rows (unique sequences).
    - int: Number of columns (same as number of rows, unique sequences).

    """
    x, seqs_u, seqs_u_inv,  n_row_u,  n_col_u  = seqs_to_ix(
        seqs=seqs,
        cpus = cpus, 
        collision = collision,
        min_occur = min_occur)

    if use_dask == False:
        print(f"{clock()} -- Getting edges by permutations")
        t0 = time.perf_counter()
        ij_u = get_permutations(x)
        ii_u = get_identity(n_row_u)
        ij_u = pd.concat([ii_u, ij_u])
        memory_usage = get_memory_usage()
        print(f"{clock()} -- Current memory usage: {memory_usage / (1024 ** 2):.2f} MB")
        ij_u = ij_u.drop_duplicates()
        
        t1 = time.perf_counter()
        total = t1-t0
        print(f"{clock()} -- BASIC TIME {total:0.4f} sec")
        print(f"{clock()} -- Completed permutations -- {total:0.4f} sec")
        print(f"{clock()} -- Converting to a csr_matrix")

    if use_dask:
        print(f"{clock()} -- USING DASK")
        import dask.dataframe as dd
        from dask.distributed import Client, LocalCluster
        from dask.distributed import performance_report
        cluster = LocalCluster(n_workers=cpus, 
            threads_per_worker=1, 
            memory_limit = memory_limit)
        client = Client(cluster)

        ddf = dd.from_pandas(x, npartitions=cpus)
        ddf = ddf.set_index('h')
        # Apply the function to each partition
        result = ddf.map_partitions(get_permutations, 
            meta=pd.DataFrame({'i': pd.Series(dtype='uint32'), 
                'j': pd.Series(dtype='uint32')}))

        t0 = time.perf_counter()
        with performance_report(filename=dask_report_name):
            ij_u= result.compute()
        t1 = time.perf_counter()
        total = t1-t0
        print(f"{clock()} -- Completed permutations with DASK-- {total:0.4f} sec")
        
        client.close()
        cluster.close()
        
        print(f"{clock()} -- Dropping Duplicates")
        ii_u = get_identity(n_row_u)
        ij_u = pd.concat([ii_u, ij_u])
        ij_u = ij_u.drop_duplicates()

    # WARNING: return_raw = True IS ONLY FOR ADVANCED USERS, 
    # NO REDUPLICATION IS PERFORMED, WARNING ORDER DOES NOT MATCH INPUT
    if return_raw:
        return ij_u, ii_u, seqs_u, seqs_u_inv, n_row_u, n_col_u
    #import pdb; pdb.set_trace()
    # FOR USER WHO WANTS FULL CSR_MAT MATCHING INPUT ORDER AND SIZE
    data = np.ones(ij_u.shape[0], dtype = 'int8')
    S1 = csr_matrix((data, (ij_u['i'].values, ij_u['j'].values)), shape=(n_row_u, n_col_u))
    data0 = np.ones(ii_u.shape[0], dtype = 'int8')
    S0 = csr_matrix((data0, (ii_u['i'].values, ii_u['j'].values)), shape=(n_row_u, n_col_u))
    print(f"{clock()} -- Shape of unique csr_mat edit1 {S1.shape}")
    print(f"{clock()} -- Shape of unique csr_mat edit0 {S0.shape}")
    print(f"{clock()} -- Expanding to csr_matrix to original dimensions")
    # CONSIDER FUTURE VERSION WHERE YOU DON'T REXPAND
    try:
        S1 = S1[seqs_u_inv,:][:,seqs_u_inv]
        S0 = S0[seqs_u_inv,:][:,seqs_u_inv]
    except ValueError:
        # Sometimes the sparse matrix can't handle this large of slice
        # so do half slice at a time
        # hx = int(seqs_u_inv.shape[0] / 2)
        # from scipy.sparse import hstack
        # parta   = S1[seqs_u_inv,:][:,seqs_u_inv[0:hx]]
        # partb   = S1[seqs_u_inv,:][:,seqs_u_inv[hx:]]
        # S1 = hstack((parta, partb))
        # parta   = S0[seqs_u_inv,:][:,seqs_u_inv[0:hx]]
        # partb   = S0[seqs_u_inv,:][:,seqs_u_inv[hx:]]
        # S0 = hstack((parta, partb))
        S1 = expand_a_csr(S=S1, seqs_u_inv=seqs_u_inv)
        S0 = expand_a_csr(S=S0, seqs_u_inv=seqs_u_inv)
    print(f"{clock()} -- Shape of expanded csr_mat edit1 {S1.shape}")
    print(f"{clock()} -- Shape of expanded csr_mat edit0 {S0.shape}")
    
    if min_nn is not None:
        from tcrtest.sparse import set_rows_to_sparse
        print(f"{clock()} -- sparsifying rows with fewer than {N} entries")
        S1 = set_rows_to_sparse(S1, N = min_nn)

    return S0, S1

def main():
    parser = argparse.ArgumentParser(description="Process some integers.")
    
    # Required positional argument
    parser.add_argument('--infile', type=str, help='The path to the input file')

    # Optional arguments
    parser.add_argument('--sep', type=str, default=",", help='sep for infile (default .csv)')
    parser.add_argument('--seqs_col', type=str, default="amino_acid", help='Column name to use (default: amino_acid)')
    parser.add_argument('--cpus', type=int, default=2, help='Number of CPUs to use (default: 20)')
    parser.add_argument('--use_dask', action='store_true', help='Flag to indicate if Dask should be used')
    parser.add_argument('--outfile_csrmat_edit0', type=str, default='test.edit0.npz', help='Output file name for csr matrix edit 0')
    parser.add_argument('--outfile_csrmat_edit1', type=str, default='test.edit1.npz', help='Output file name for csr matrix edit 1')
    parser.add_argument('--memory_limit', type=str, default="20GB", help='Memory limit for each work in Dask the process (default: 20GB)')
    parser.add_argument('--min_occur', type=int, default=1, help='drop degenerate key if doesnt appear more than min_occur')
    parser.add_argument('--dask_report_name', type=str, default="dask-report-3.html", help='Filename for the Dask HTML report (default: dask-report-3.html)')

    # Parse arguments
    args = parser.parse_args()

    print(f"Filename: {args.infile}")
    print(f"Using Dask: {args.use_dask}")
    if args.use_dask:
        print(f"Dask report will be saved as: {args.dask_report_name}")

    seqs_df = pd.read_csv(args.infile, sep = args.sep)
    print(f"{clock()} -- Input: number of rows {seqs_df.shape[0]}")
    seqs = seqs_df[ args.seqs_col ]
    print(f"{clock()} -- opening {os.path.basename(args.infile)}")
    if args.use_dask:
        print(f"{clock()}  --- [seqs_to_csr] w/dask mode") 
        t0 = time.perf_counter()
        S0, S1 = seqs_to_csr(seqs, cpus = args.cpus, use_dask = True,
            dask_report_name = args.dask_report_name,
            memory_limit = args.memory_limit)
        t1 = time.perf_counter()
        total = t1-t0
        print(f"{clock()}  --- {total:0.4f} seconds Total Time using Dask and {args.cpus} cpus")
    else:
        t0 = time.perf_counter()
        S0, S1 = seqs_to_csr(seqs, cpus = args.cpus, use_dask = False)
        t1 = time.perf_counter()
        total = t1-t0
        print(f"{clock()}  --- {total:0.4f} seconds Total Time using 1 cpus to find permutations")
    print(f"{clock()} -- writing {args.outfile_csrmat_edit0}")
    save_npz(args.outfile_csrmat_edit0, S0, compressed = False)
    print(f"{clock()} -- writing {args.outfile_csrmat_edit1}")
    save_npz(args.outfile_csrmat_edit1, S1, compressed = False)

if __name__ == "__main__":
    main()








# TESTING THAT DASK AND BASIC MODE AGREE
# v = "V13"
# args.filename= f'/fh/scratch/delete90/gilbert_p/20240411_emerson_split50/stratv_combined/{v}_combined.CLEAN.csv'
# print(f"{clock()} -- opening {os.path.basename(args.filename)}")
# seqs_df = pd.read_csv(args.filename)
# print(f"{clock()} -- Input: number of rows {seqs_df.shape[0]}")
# seqs = seqs_df['amino_acid']
# t0 = time.perf_counter()
# S0, S1 = seqs_to_csr(seqs, cpus = args.cpus, use_dask = False)
# t1 = time.perf_counter()
# total = t1-t0
# print(f" --- {total:0.4f} seconds Total Time using 1 cpus to find permutations")
# t0 = time.perf_counter()
# S0dask, S1dask = seqs_to_csr(seqs, cpus = args.cpus, use_dask = True)
# t1 = time.perf_counter()
# total = t1-t0
# print(f" --- {total:0.4f} seconds Total Time using Dask and {args.cpus} cpus")
# from tcrtest.checks import are_csr_matrices_identical
# print(f"{clock()} -- Testing that Dask and Basic results agree")
# assert are_csr_matrices_identical(S0, S0dask)
# assert are_csr_matrices_identical(S1, S1dask)
# print(f"{clock()} -- Dask and Basic results agree")


# from rapidfuzz.distance.Levenshtein import distance as levenshtein
# import numpy as np
# from scipy.sparse import dok_matrix
# import pandas as pd
# from scipy.sparse import dok_matrix
# def bf(seqs, seqs2, max_dist = 1):
#     n_row = len(seqs)
#     n_col = len(seqs2)
#     S = dok_matrix((n_row, n_col), dtype=np.int8)
#     for i in range(n_row):
#         for j in range(n_col):
#             d = levenshtein(seqs[i],seqs2[j])
#             if d <= max_dist:
#                 S[i,j]=1
#     return S.tocsr()