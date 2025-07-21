import numpy as np 
import pandas as pd
from scipy.sparse import csr_matrix
from datetime import datetime


def clock():
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S")
    return current_time
    
def split_into_chunks(lst, max_chunk_size):
    # Split the list into chunks
    chunks = [lst[i:i + max_chunk_size] for i in range(0, len(lst), max_chunk_size)]
    return chunks

def get_nieghbor_index_generator(csr_mat):
    for x in np.split(csr_mat.indices, csr_mat.indptr[1:-1]):
        yield list(x)

def get_nieghbor_index(csr_mat):
    return [list(x) for x in np.split(csr_mat.indices, csr_mat.indptr[1:-1])]

def matrix_tab(M=None,p=None,idx=None,conditional_column = None, subset_column = None):
    """
    M : matrix of booleans
    p : vector aligned with matrix encoding rows associated with a TCR feature neighborhood
    conditional_column : int or None (position of column to condution on)
    subset_column : int or None (position of column to retain if True, used for dropping subsets of the matrix)


    if testing_only:
        # Formal example - only for testing
        # Suppose that have n people who either have a biomarker of interest (1) or not (0).
        # For each biomarker we will represent n people as a stacked vector array of shape nx1. 
        # Thus for k biomarkers, we repressent the cohort by an array <p> which has dimension 
        # (k, n, 1), e.g., (2,8,1)
        if p is None:
            p = np.array([
                np.vstack([1,0,0,1,0,0,0,0]),
                np.vstack([1,1,1,1,1,1,1,1])])
        # Next we consider some binary covariates that describe the n members of the cohort. 
        # This could be whether the person has a particular HLA allele, or whether they 
        # present a disease. The binary covariates matrix is <M> and it has dimensions 
        # (n,m) e.g., (8,4) for n=8 persons, and m=4 covariates
        if M is None:
            M = np.array([[1,1,0,0],
                        [1,0,1,0],
                        [1,0,0,1],
                        [1,1,1,0],
                        [0,1,0,0],
                        [0,0,0,1],
                        [0,1,0,0],
                        [0,0,1,1]])

    """
    # IF USER PASSES 2D ARRAY INSTEAD OF VSTACK
    if p.ndim == 2:   
        print("REFORMATING TO VSTACK FORMAT")
        #Convert each row to a column vector (3D array)
        p = np.array([row[:, np.newaxis] for row in p])

    # Suppose we wish to tabulate, conditional only on participants 
    # (+) for some covariate
    if subset_column is not None:
        print(f"Subset applied based on {subset_column}")
        Ms = M[:,subset_column] 
        M = M[Ms == 1 ]
        p = np.array([np.vstack(pi[Ms==1]) for pi in p])

    if conditional_column is not None:
        print(f"Conditionality applied based on {conditional_column}")
        Mc = M[:,conditional_column ] 
        M = M[Mc ==1 ]
        p = np.array([np.vstack(pi[Mc==1]) for pi in p])
    
    # These arrays permit the efficient numpy opperations and creation of an 3D array
    # X has dimensions (k,n,m) e.g., (2, 8, 4)
    X = p*M
    # Simililary we create a matrix for all the individuals lacking the biomarkres
    Xi = (p == 0)*M
    # We can then sum across the axis = 1 to identify how many individuals 
    # positive and negative for each biomarkers based covariate values
    # <S> is the number biomarker (+) AND coveriate (+)
    S = X.sum(axis = 1).astype('uint32')
    #assert np.all(S == np.array([[2, 2, 1, 0],[4, 4, 3, 3]]))
    # <Si # is the number biomarker (-) AND coveriate (+)
    Si= Xi.sum(axis = 1).astype('uint32')
    #assert np.all(Si == np.array([[2, 2, 2, 3],[0, 0, 0, 0]]))
    # <Np> is the number of biomarker(+) in total
    Np = p.sum(axis =1).astype('uint32')
    #assert np.all(Np == np.array([[2],[8]]))
    # <Ni> is the number of biomarker(-) in total
    Ni = (p == 0).sum(axis=1).astype('uint32')
    #assert np.all(Ni == np.array([[6],[0]]))
    # We Then subtract S, and Si from Np and Ni, respectively to get
    # R and Ri
    # <R>, which is the number covariate (+) / biomarker (-)  
    R = (Np - S).astype('uint32')
    # <Ri> which is the number covariate (-) / biomarker (-)  
    Ri = (Ni - Si).astype('uint32')
    
    if conditional_column is not None:
        total_biomarker_pos = np.tile(Np, S.shape[1])
        total_covariate_pos = np.tile(M.sum(axis = 0), S.shape[0])
    else:
        total_biomarker_pos = np.tile(Np,S.shape[1])
        total_covariate_pos = np.tile(M.sum(axis = 0), S.shape[0])

    
    # This permits the construction of long-form tabular result
    df = pd.DataFrame({
        'a':np.concatenate(S),
        'b':np.concatenate(R),
        'c':np.concatenate(Si),
        'd':np.concatenate(Ri)})
    # To record the correct index of each biomarker we us the input vector idx
    #if idx is None:
    #    idx = [10,100]
    ix = [[idx[x]]*M.shape[1] for x in range(p.shape[0])]
    df['i'] =  pd.Series(np.concatenate(ix),  dtype = 'uint32')
    # To record the covriate index we assign m
    df['m'] = pd.Series([i for i in range(M.shape[1])]*p.shape[0], dtype = 'uint32')
    return(df)


def matrix_tab_w_missing_info(M=None, p=None, idx=None, conditional_column=None, subset_column=None):
    """
    Computes tabulated counts of individuals based on biomarkers and covariates, handling NaN values.

    This function calculates counts of individuals who are positive or negative for certain biomarkers
    and covariates, optionally applying subset or conditional filters. It handles NaN values in the
    covariate matrix `M` appropriately by excluding them from the calculations.

    Parameters
    ----------
    M : ndarray of shape (n, m)
        Covariate matrix for `n` individuals and `m` covariates. Entries can be:
        - `1` for covariate presence (positive)
        - `0` for covariate absence (negative)
        - `NaN` for missing data (excluded from calculations)

    p : ndarray of shape (k, n)
        Biomarker matrix for `k` biomarkers and `n` individuals. Entries should be:
        - `1` for biomarker presence (positive)
        - `0` for biomarker absence (negative)

    idx : list or array-like of length `k`, optional
        Identifiers or indices for the biomarkers. If `None`, indices from `0` to `k - 1` are used.

    conditional_column : int, optional
        Index of the covariate column in `M` to condition on. If specified, only individuals where
        this covariate is present (`1`) and not `NaN` are included in the calculations.

    subset_column : int, optional
        Index of the covariate column in `M` to subset on. If specified, only individuals where
        this covariate is present (`1`) and not `NaN` are retained for the analysis.

    Returns
    -------
    df : pandas.DataFrame
        A DataFrame containing the counts for each biomarker and covariate combination. The columns are:
        - `'a'`: Number of individuals who are biomarker positive and covariate positive.
        - `'b'`: Number of individuals who are biomarker positive and covariate negative.
        - `'c'`: Number of individuals who are biomarker negative and covariate positive.
        - `'d'`: Number of individuals who are biomarker negative and covariate negative.
        - `'i'`: Biomarker index or identifier (from `idx` if provided).
        - `'m'`: Covariate index.

    Examples
    --------
    >>> import numpy as np
    >>> import pandas as pd
    >>> p = np.array([
    ...     [1, 0, 0, 1, 0, 0, 0, 0],
    ...     [1, 1, 1, 1, 1, 1, 1, 1]
    ... ])
    >>> M = np.array([
    ...     [1, 1, np.nan, 0],
    ...     [1, 0, 1, 0],
    ...     [1, 0, 0, 1],
    ...     [1, 1, 1, 0],
    ...     [0, 1, 0, 0],
    ...     [0, 0, 0, 1],
    ...     [0, 1, 0, 0],
    ...     [0, 0, 1, 1]
    ... ])
    >>> df = matrix_tab(M=M, p=p)
    >>> print(df)
       a  b  c  d  i  m
    0  2  0  2  0  0  0
    1  1  0  1  0  0  1
    2  0  0  1  0  0  2
    3  1  0  1  0  0  3
    4  7  1  1  0  1  0
    5  5  1  1  0  1  1
    6  4  1  1  0  1  2
    7  3  1  1  0  1  3

    Notes
    -----
    - **NaN Handling**: NaN values in `M` are treated as missing data and are excluded from all counts and calculations.
    - **Counts Explanation**:
      - `'a'`: Count of individuals who are biomarker positive (`p == 1`) and covariate positive (`M == 1`).
      - `'b'`: Count of individuals who are biomarker positive (`p == 1`) and covariate negative (`M == 0`).
      - `'c'`: Count of individuals who are biomarker negative (`p == 0`) and covariate positive (`M == 1`).
      - `'d'`: Count of individuals who are biomarker negative (`p == 0`) and covariate negative (`M == 0`).
    - **Filtering**:
      - If `conditional_column` is specified, only individuals where the specified covariate is present are included.
      - If `subset_column` is specified, only individuals where the specified covariate is present are retained.
    - **Indices**:
      - Biomarker indices (`'i'`) correspond to the rows in `p` or entries in `idx`.
      - Covariate indices (`'m'`) correspond to the columns in `M`.
"""
   
    # IF YOU HAVE A VSTACKED INPUT CONVERT
    if p.ndim == 3 and p.shape[2] == 1:
        p = np.array([np.squeeze(arr) for arr in p])

    M = np.asarray(M, dtype=float)
    # Convert None values to np.nan
    M = np.where(M == None, np.nan, M)
    

    # Apply subset condition if specified
    if subset_column is not None:
        print(f"Subset applied based on {subset_column}")
        Ms = M[:, subset_column]
        # Exclude rows where Ms is NaN
        valid_indices = (Ms == 1) & (~np.isnan(Ms))
        M = M[valid_indices]
        p = p[:, valid_indices]
    
    # Apply conditionality if specified
    if conditional_column is not None:
        print(f"Conditionality applied based on {conditional_column}")
        Mc = M[:, conditional_column]
        # Exclude rows where Mc is NaN
        valid_indices = (Mc == 1) & (~np.isnan(Mc))
        M = M[valid_indices]
        p = p[:, valid_indices]
    
    # Create a mask of valid (non-NaN) entries in M
    mask_M = ~np.isnan(M)  # Shape: (n, m)
    
    k, n = p.shape  # k is number of biomarkers, n is number of individuals
    m = M.shape[1]  # m is number of covariates
    
    # Expand p to shape (k, n, m)
    p_expanded = p[:, :, np.newaxis]  # Shape: (k, n, 1)
    p_expanded = np.tile(p_expanded, (1, 1, m))  # Shape: (k, n, m)
    
    # Expand mask_M to match p_expanded
    mask_M_expanded = mask_M[np.newaxis, :, :]  # Shape: (1, n, m)
    
    # Compute Np and Ni, accounting for valid data positions
    Np = np.sum((p_expanded == 1) & mask_M_expanded, axis=1).astype('uint32')  # Shape: (k, m)
    Ni = np.sum((p_expanded == 0) & mask_M_expanded, axis=1).astype('uint32')  # Shape: (k, m)
    
    # Replace NaNs in M with zeros for calculations
    M_masked = np.where(mask_M, M, 0)  # Shape: (n, m)
    M_masked = M_masked[np.newaxis, :, :]  # Shape: (1, n, m)
    
    # Compute X and Xi
    X = ((p_expanded == 1) * M_masked).astype('float')  # Shape: (k, n, m)
    Xi = ((p_expanded == 0) * M_masked).astype('float')  # Shape: (k, n, m)
    
    # Compute S and Si
    S = np.sum(X, axis=1).astype('uint32')  # Shape: (k, m)
    Si = np.sum(Xi, axis=1).astype('uint32')  # Shape: (k, m)
    
    # Compute R and Ri
    R = Np - S  # Shape: (k, m)
    Ri = Ni - Si  # Shape: (k, m)
    
    # Flatten the arrays for DataFrame construction
    df = pd.DataFrame({
        'a': np.concatenate(S).astype('int16'),
        'b': np.concatenate(R).astype('int16'),
        'c': np.concatenate(Si).astype('int16'),
        'd': np.concatenate(Ri).astype('int16')
    })
    
    # Assign biomarker indices
    if idx is None:
        idx = list(range(k))
    ix = [[idx[x]] * m for x in range(k)]
    try:
        df['i'] = pd.Series(np.concatenate(ix), dtype='uint32')
    except ValueError:
        df['i'] = pd.Series(np.concatenate(ix))
    # Assign covariate indices
    df['m'] = pd.Series(list(range(m)) * k, dtype='uint32')
    
    return df



    
def iterable_hla_w_missing_info(x, 
    sample_vector, 
    binary_matrix, 
    conditional_column= None, 
    subset_column = None):
    """
    * Suppose we have a nxk vectors of TCRs 
    * Suppose we have a nxk vector subjects associated with each TCR
    This is used to set up function <matrix_tab>
    x : (tuple) part 0 is a list of index positions of TCR sequence, part 1 is neighbor index of its neighbros
    subject_vector : subject_vector
    x_hla : (np.array)
    subset_column : (str)
    condtional_column : (str)


    Notes:
    # For each index TCR a neighbor set exists. For instance TCRi
    # might have neighbors [1,5,6] , which in pp will map to 
    # "sample_a", "sample_a", "sample_b"
    # >>> These sample names must perfectly matrix teh binary matrix index
    # Note: for each neighbor set, we are getting all sample names
    # Such that we can then create <p> stacked vectors for each index TCR
    # based on whether that participant has TCR or not
    """
    idx = x[0]
    nnx = x[1]

    pp = [sample_vector.iloc[nn] for nn in nnx]
    p =  np.array([np.vstack(binary_matrix.index.isin(x).astype(int)) for x in pp])
    
    result = matrix_tab_w_missing_info(M = binary_matrix.values, 
        p = p, 
        idx = idx, 
        conditional_column= conditional_column, 
        subset_column = subset_column)

    return(result)

    
def iterable_hla_function(x, 
    sample_vector, 
    binary_matrix, 
    conditional_column= None, 
    subset_column = None):
    """
    * Suppose we have a nxk vectors of TCRs 
    * Suppose we have a nxk vector subjects associated with each TCR
    This is used to set up function <matrix_tab>
    x : (tuple) part 0 is a list of index positions of TCR sequence, part 1 is neighbor index of its neighbros
    subject_vector : subject_vector
    x_hla : (np.array)
    subset_column : (str)
    condtional_column : (str)


    Notes:
    # For each index TCR a neighbor set exists. For instance TCRi
    # might have neighbors [1,5,6] , which in pp will map to 
    # "sample_a", "sample_a", "sample_b"
    # >>> These sample names must perfectly matrix teh binary matrix index
    # Note: for each neighbor set, we are getting all sample names
    # Such that we can then create <p> stacked vectors for each index TCR
    # based on whether that participant has TCR or not
    """
    idx = x[0]
    nnx = x[1]

    pp = [sample_vector.iloc[nn] for nn in nnx]
    p =  np.array([np.vstack(binary_matrix.index.isin(x).astype(int)) for x in pp])
    
    result = matrix_tab(M = binary_matrix.values, 
        p = p, 
        idx = idx, 
        conditional_column= conditional_column, 
        subset_column = subset_column)

    return(result)