import parmap
import numpy as np 
import pandas as pd
from tcrtest.testing import matrix_tab_w_missing_info
from tcrtest.testing import split_into_chunks
import os
from scipy.sparse import dok_matrix, csr_matrix
from tcrtest.similar import get_multimer_dictionary
from tcrtest.similar import get_query_v_subject_npz_dok

def iterable_func(x, 
    binary_matrix, 
    conditional_column= None, 
    subset_column = None):
    """
    Process a subset of a binary matrix based on given conditions.

    Parameters
    ----------
    x : tuple
        A tuple where the first element is an index and the second element is a DataFrame.
    binary_matrix : pandas.DataFrame
        A binary matrix representing presence/absence data.
    conditional_column : str, optional
        Column used for conditional selection (default is None).
    subset_column : str, optional
        Column used for subset selection (default is None).

    Returns
    -------
    pandas.DataFrame
        Processed data matrix with missing values handled.
    """

    idx = x[0]
    nnx = x[1].values
    p = nnx 
    result = matrix_tab_w_missing_info(M = binary_matrix.values, 
        p = p, 
        idx = idx, 
        conditional_column= conditional_column, 
        subset_column = subset_column)
    return(result)

def iterable_fisher(x1):
    """
    Perform Fisher's Exact Test on a DataFrame.

    Parameters
    ----------
    x1 : pandas.DataFrame
        A DataFrame containing columns 'a', 'b', 'c', and 'd'.

    Returns
    -------
    pandas.DataFrame
        The original DataFrame with additional columns for odds ratios and p-values.
    """
    original_index = x1.index
    x1 = x1.reset_index(drop = True)
    from fishersapi import fishers_vec
    x1['a'] = x1['a'].fillna(0).astype('int64')
    x1['b'] = x1['b'].fillna(0).astype('int64')
    x1['c'] = x1['c'].fillna(0).astype('int64')
    x1['d'] = x1['d'].fillna(0).astype('int64')
    x1['odds'], x1['p_est'] = fishers_vec(x1.a+1,x1.b+1,x1.c+1,x1.d+1)
    x1['odds_exact'], x1['p_exact'] = fishers_vec(x1.a,x1.b,x1.c,x1.d)
    x1.index = original_index
    return(x1)

def tab(fp, query, sep = ",", get_col = "templates",on = 'vfamcdr3', min_value = None):
    """
    Aggregate template counts based on a grouping column.

    Parameters
    ----------
    fp : str
        File path to a CSV or TSV file.
    query : pandas.DataFrame
        DataFrame containing the query data.
    sep : str, optional
        Delimiter used in the input file (default is ',').
    get_col : str, optional
        Column name containing the values to aggregate (default is 'templates').
    on : str, optional
        Column name to group data by (default is 'vfamcdr3').
    min_value : int, optional
        Minimum value threshold for filtering (default is None).

    Returns
    -------
    pandas.DataFrame
        Aggregated results merged with query data.
    """
    dx = pd.read_csv(fp, sep = sep)
    if min_value is not None:
        dx = dx[dx[get_col] > min_value].reset_index(drop = True)
    
    dxt = dx.groupby(on).sum()
    result = query.merge(dxt, how = "left", on = on).fillna(0)
    assert result.shape[0] == query.shape[0]
    name = os.path.basename(fp).replace(".tsv","").replace(".csv","")
    rt = pd.DataFrame({name: result[get_col]})
    return rt

def tab1(fp, query, sep = ",", get_col = "templates",on = 'vfamcdr3', min_value = None, enforcement = True):
    """
    Aggregate template counts based on a grouping column with optional enforcement rules.

    Parameters
    ----------
    fp : str
        File path to a CSV or TSV file.
    query : pandas.DataFrame
        DataFrame containing the query data.
    sep : str, optional
        Delimiter used in the input file (default is ',').
    get_col : str, optional
        Column name containing the values to aggregate (default is 'templates').
    on : str, optional
        Column name to group data by (default is 'vfamcdr3').
    min_value : int, optional
        Minimum value threshold for filtering (default is None).
    enforcement : bool, optional
        Whether to apply enforcement rules for Vfam consistency upon identical CDR3 (default is True).

    Returns
    -------
    pandas.DataFrame
        Aggregated results merged with query data, with enforcement applied if enabled.
    """
    # make sure we aren't overwriting anything when we apply enforcement
    query1 = query.copy()
    dx = pd.read_csv(fp, sep = sep)
    #print(dx)
    if min_value is not None:
        dx = dx[dx[get_col] > min_value].reset_index(drop = True)
    dxt = dx.groupby(on).sum().reset_index(drop = False)
    
    # enforcement uses a simplyrick to avoid different V exact CDR by converting V02CAS to V02V02CAS
    if enforcement:
        query1[on] = query1[on].str[0:3] + query1[on] # (We add extract V03)
        dxt[on] = dxt[on].str[0:3] + dxt[on]
    
    dq = get_multimer_dictionary(random_strings = query1[on], 
    trim_left = None, trim_right = None)

    ds = get_multimer_dictionary(random_strings = dxt[on], 
        trim_left = None, trim_right = None, 
        conserve_memory = False)
    
    csr_mat1 = get_query_v_subject_npz_dok(
        dq=dq, 
        ds=ds, 
        n_rows = query1.shape[0], 
        n_cols = dxt.shape[0], 
        nn_min = 0)

    n_rows = query1.shape[0]
    n_cols = dxt.shape[0]
    dk           = dok_matrix((n_rows, n_cols))
    values = dxt[get_col]
    for (i,j),_ in csr_mat1.todok().items():
        dk[(i,j)] = values.iloc[j]
    csr_mat_val = dk.tocsr()
    result = pd.Series(csr_mat_val.sum(axis=1).A1)
    name = os.path.basename(fp).replace(".tsv","").replace(".csv","")
    rt = pd.DataFrame({name: result})
    return rt


def tabnr(fp, query, sep = ",", get_col = "templates",on = 'vfamcdr3', min_value = None,
    group_indices = None, count_unique = False, as_breadth = False):

    """
    Aggregate and summarize template counts with optional grouping and uniqueness constraints.

    Parameters
    ----------
    fp : str
        File path to a CSV or TSV file.
    query : pandas.DataFrame
        DataFrame containing the query data.
    sep : str, optional
        Delimiter used in the input file (default is ',').
    get_col : str, optional
        Column name containing the values to aggregate (default is 'templates').
    on : str, optional
        Column name to group data by (default is 'vfamcdr3').
    min_value : int, optional
        Minimum value threshold for filtering (default is None).
    group_indices : array-like, optional
        Grouping indices for summarization (default is None).
    count_unique : bool, optional
        Whether to count unique clones instead of summing values (default is False).
    as_breadth : bool, optional
        Normalize results by total clone count if True (default is False).

    Returns
    -------
    pandas.DataFrame
        Aggregated results with optional grouping and uniqueness constraints.
    """
    dx = pd.read_csv(fp, sep = sep)
    if min_value is not None:
        dx = dx[dx[get_col] > min_value].reset_index(drop = True)
    total_clones = dx.shape[0]

    if count_unique:
        # here is if we want to know how many clones 
        dx['unique'] = (dx[get_col] > 0).astype(int)
        dxt = dx.groupby(on).sum()
        result = query.merge(dxt, how = "left", on = on).fillna(0)
        assert result.shape[0] == query.shape[0]
        if group_indices is not None:
            result['group_indices'] = group_indices
            result_sum = result.groupby(group_indices)['unique'].sum().to_dict()
        else:
            result_sum  = {'All': result['unique'].sum()}
        
        if as_breadth:
            result_sum = {k:v/total_clones for k,v in result_sum.items()}

        name = os.path.basename(fp).replace(".tsv","").replace(".csv","")
        rt = pd.DataFrame({name: result_sum})

    else:
        dxt = dx.groupby(on).sum()
        result = query.merge(dxt, how = "left", on = on).fillna(0)
        assert result.shape[0] == query.shape[0]
        
        if group_indices is not None:
            result['group_indices'] = group_indices
            result_sum = result.groupby(group_indices)[get_col].sum().to_dict()
        else:
            result_sum  = {'All': result[get_col].sum()}
        name = os.path.basename(fp).replace(".tsv","").replace(".csv","")
        rt = pd.DataFrame({name: result_sum})
    

    return rt

def tab1nr(fp, query, sep = ",", get_col = "templates",on = 'vfamcdr3', min_value = None, 
    enforcement = True, group_indices = None, count_unique = False, as_breadth = False):
    """
    Computes the non-redundant sum of a specified column across grouped entries from a CSV file,
    enforcing unique identifiers if necessary.

    Parameters
    ----------
    fp : str
        File path to the CSV file.
    query : pd.DataFrame
        DataFrame containing the query data.
    sep : str, optional
        Delimiter used in the CSV file (default is ',').
    get_col : str, optional
        Column to sum over (default is 'templates').
    on : str, optional
        Column used for grouping (default is 'vfamcdr3').
    min_value : int or None, optional
        Minimum threshold for filtering values (default is None).
    enforcement : bool, optional
        Whether to enforce unique identifiers in the grouping column (default is True).
    group_indices : array-like or None, optional
        Indices used for grouping when computing the non-redundant sum (default is None).
    count_unique : bool, optional
        Whether to count unique occurrences instead of summing values (default is False).
    as_breadth : bool, optional
        Whether to normalize by total unique clones when counting unique occurrences (default is False).

    Returns
    -------
    pd.DataFrame
        A DataFrame containing the computed non-redundant sum values grouped by indices or overall.
    """
    # For getting non redundant sum
    # make sure we aren't overwriting anything when we apply enforcement
    query1 = query.copy()
    dx = pd.read_csv(fp, sep = sep)
    if min_value is not None:
        dx = dx[dx[get_col] > min_value].reset_index(drop = True)
    dxt = dx.groupby(on).sum().reset_index(drop = False)
    
    total_clones = dx.shape[0]
    # enforcement uses a simple trick to avoid different V exact CDR by converting V02CAS to V02V02CAS
    if enforcement:
        query1[on] = query1[on].str[0:3] + query1[on] # (We add extra V03)
        dxt[on] = dxt[on].str[0:3] + dxt[on]
    
    dq = get_multimer_dictionary(random_strings = query1[on], 
    trim_left = None, trim_right = None)

    ds = get_multimer_dictionary(random_strings = dxt[on], 
        trim_left = None, trim_right = None, 
        conserve_memory = False)
    
    csr_mat1 = get_query_v_subject_npz_dok(
        dq=dq, 
        ds=ds, 
        n_rows = query1.shape[0], 
        n_cols = dxt.shape[0], 
        nn_min = 0)
    # Suppose we only want to know non_redundant sum


    n_rows = query1.shape[0]
    n_cols = dxt.shape[0]
    dk           = dok_matrix((n_rows, n_cols))
    values = dxt[get_col]
    
    if count_unique:

        if as_breadth:
            # count each divdided by total unique clones (above min_val threshold)
            for (i,j),_ in csr_mat1.todok().items():
                dk[(i,j)] = 1/total_clones
        else:
            # Here we just want to know how many hits there where
            for (i,j),_ in csr_mat1.todok().items():
                dk[(i,j)] = 1

    else:
        # Here we care about values specified by <get_col>, see above
        for (i,j),_ in csr_mat1.todok().items():
            dk[(i,j)] = values.iloc[j]
    
    csr_mat_val = dk.tocsr()
    
    if group_indices is not None:
        unique_groups = np.unique(group_indices)
        group_sums = {}
        for group in unique_groups:
            # Find the rows for the current group
            rows = np.where(group_indices == group)[0]
            # Slice the matrix to get the group's submatrix
            group_matrix = csr_mat_val[rows, :]
            # Compute the maximum in each column for the group
            # .max(axis=0) returns a 1xN matrix; .A1 converts it to a flattened array
            max_vals = group_matrix.max(axis=0)
            # Sum the max values to get the non-redundant sum for this group
            group_sum = max_vals.sum()
            if as_breadth:
                group_sum = group_sum / total_clones
            group_sums[group] = group_sum

    else:
        group_sums = {'All': csr_mat_val.max(axis=0).sum()}


    name = os.path.basename(fp).replace(".tsv","").replace(".csv","")
    rt = pd.DataFrame({name: group_sums})
    return rt

import pandas as pd
import re
from scipy.sparse import lil_matrix
import numpy as np
import os
import re

def tab_regex(fp, 
    query, 
    sep=",", 
    regex_col= "regex", 
    get_col="templates", 
    on="vfamcdr3", 
    min_value=None, 
    unique_hits = False):
    """
    For each regex pattern in the query, sum the get_col values in the file
    for rows that match the regex pattern.

    Parameters
    ----------
    fp : str
        Path to CSV/TSV file with at least `on` and `get_col` columns.
    query : pandas.DataFrame
        DataFrame with regex patterns in the `on` column.
    sep : str, optional
        Delimiter used in the input file (default is ',').
    get_col : str, optional
        Column name to sum for each regex match (default is 'templates').
    on : str, optional
        Column name containing regex patterns (default is 'vfamcdr3').
    min_value : float or int, optional
        Keep only rows in the input file where get_col > min_value.

    Returns
    -------
    pandas.DataFrame
        Single-column DataFrame with same index as query, containing sum of get_col
        values from the file for each regex pattern. Column is named after the file.
    """
    df = pd.read_csv(fp, sep=sep)
    df = df[df[on].notna()].reset_index(drop=True) # remove potential for na column
    if min_value is not None:
        df = df[df[get_col] > min_value].reset_index(drop=True)

    patterns = query[regex_col].values
    strings = df[on].values
    if on == "vfamcdr3":
        #print("Accerlating with V partition")
        vstrings = [x[0:3] for x in strings]
    weights = df[get_col].values
    if unique_hits:
        #print("Ignoring <get_col> and instead tabulating unique hits")
        df['u'] = 1
        weights = df['u'].values

    vs = pd.Series(vstrings)
    ss = pd.Series(strings)
    ws = pd.Series(weights)

    results = []
    for pat in patterns:
        rx = re.compile(pat)
        vpat = pat[0:3]
        #temp_strings = [x for v,x in zip(vstrings, strings) if v == vpat ]
        #temp_weights = [x for v,x in zip(vstrings, weights) if v == vpat ] 
        #total = sum(w for s, w in zip(temp_strings, temp_weights) if rx.fullmatch(s))
        # to, v2
        # first get VFAM match, to reduce total number of regex.
        ix = vs == vpat
        match_mask = ss[ix].str.fullmatch(rx.pattern)  # <- vectorized!
        total = ws[ix][match_mask].sum()
        results.append(total)

    colname = os.path.basename(fp).replace(".tsv", "").replace(".csv", "")
    return pd.DataFrame({colname: results}, index=query.index)

def tabify_regex(query, filelist, 
    regex_col = "regex", 
    on = 'vfamcdr3', 
    get_col = "templates", sep = ",", 
    cpus = 4, unique_hits = False, min_value = None):
    """
    Processes multiple files and tabulates exact TCR matches based on a specified column.

    Parameters
    ----------
    query : pd.DataFrame
        DataFrame containing query data.
    filelist : list of str
        List of file paths to process.
    on : str, optional
        Column to match on (default is 'vfamcdr3') - this must be an exact match.
    get_col : str, optional
        Column to extract values from (default is 'templates',).
    sep : str, optional
        Delimiter used in the CSV files (default is ',').
    cpus : int, optional
        Number of parallel processes to use (default is 4).
    min_value : int or None, optional
        Minimum threshold for filtering values (default is None).

    Returns
    -------
    pd.DataFrame
        Concatenated DataFrame of tabulated results across all files.
    """
    print(f"Tabulating {query.shape[0]} Exact TCR in {len(filelist)} files")
    x = parmap.map(tab_regex, filelist, 
        query = query[[regex_col]], 
        regex_col = regex_col,
        on = on,
        sep = sep,
        get_col = get_col,
        unique_hits = unique_hits,
        min_value = min_value, 
        pm_pbar=True,
        pm_processes=cpus)
    X = pd.concat(x, axis = 1)
    return X

def tabify(query, filelist, on = 'vfamcdr3', get_col = "templates", sep = ",", 
    cpus = 4, min_value = None):
    """
    Processes multiple files and tabulates exact TCR matches based on a specified column.

    Parameters
    ----------
    query : pd.DataFrame
        DataFrame containing query data.
    filelist : list of str
        List of file paths to process.
    on : str, optional
        Column to match on (default is 'vfamcdr3') - this must be an exact match.
    get_col : str, optional
        Column to extract values from (default is 'templates',).
    sep : str, optional
        Delimiter used in the CSV files (default is ',').
    cpus : int, optional
        Number of parallel processes to use (default is 4).
    min_value : int or None, optional
        Minimum threshold for filtering values (default is None).

    Returns
    -------
    pd.DataFrame
        Concatenated DataFrame of tabulated results across all files.
    """
    print(f"Tabulating {query.shape[0]} Exact TCR in {len(filelist)} files")
    x = parmap.map(tab, filelist, 
        query = query[[on]], 
        on = on,
        sep = sep,
        get_col = get_col,
        min_value = min_value, 
        pm_pbar=True,
        pm_processes=cpus)
    X = pd.concat(x, axis = 1)
    return X


def tabify1(query, filelist, on = 'vfamcdr3', get_col = "templates", sep = ",", enforcement = True, cpus = 4, min_value = None):
    """
    Processes multiple files and tabulates inexact (Edit1) TCR matches
    with optional enforcement to ensure that Vfam is also
    identical when CDR3 is exact.

    Parameters
    ----------
    query : pd.DataFrame
        DataFrame containing query data.
    filelist : list of str
        List of file paths to process.
    on : str, optional
        Column to match on (default is 'vfamcdr3').
    get_col : str, optional
        Column to extract values from (default is 'templates').
    sep : str, optional
        Delimiter used in the CSV files (default is ',').
    enforcement : bool, optional
        Whether to enforce unique identifiers in the grouping column (default is True).
        If False V02CAS and V03CAS would be considered edit1 neighbors
    cpus : int, optional
        Number of parallel processes to use (default is 4).
    min_value : int or None, optional
        Minimum threshold for filtering values (default is None).

    Returns
    -------
    pd.DataFrame
        Concatenated DataFrame of tabulated results across all files.
    """

    print(f"Tabulating {query.shape[0]} Inexact TCRs in {len(filelist)} files")
    x = parmap.map(tab1, filelist, 
        query = query[[on]], 
        on = on,
        sep = sep,
        min_value = min_value,
        enforcement = enforcement,
        get_col = get_col, 
        pm_pbar=True, 
        pm_processes=cpus)
    X = pd.concat(x, axis = 1)
    return X

def tabify_nr(query, filelist, on = 'vfamcdr3', get_col = "templates", sep = ",", 
    cpus = 4, min_value = None,group_indices = None, count_unique = False, as_breadth = False):

    """
    Processes multiple files and computes non-redundant sums for exact TCR matches.

    Parameters
    ----------
    query : pd.DataFrame
        DataFrame containing query data.
    filelist : list of str
        List of file paths to process.
    on : str, optional
        Column to match on (default is 'vfamcdr3').
    get_col : str, optional
        Column to extract values from (default is 'templates').
    sep : str, optional
        Delimiter used in the CSV files (default is ',').
    cpus : int, optional
        Number of parallel processes to use (default is 4).
    min_value : int or None, optional
        Minimum threshold for filtering values (default is None).
    group_indices : array-like or None, optional
        Indices used for grouping when computing the non-redundant sum (default is None).
    count_unique : bool, optional
        Whether to count unique occurrences instead of summing values (default is False).
    as_breadth : bool, optional
        Whether to normalize by total unique clones when counting unique occurrences (default is False).

    Returns
    -------
    pd.DataFrame
        Concatenated DataFrame of computed non-redundant sums across all files.
    """

    print(f"Tabulating {query.shape[0]} Exact TCRs in {len(filelist)} files")
    x = parmap.map(tabnr, filelist, 
        query = query[[on]], 
        on = on,
        sep = sep,
        min_value = min_value,
        get_col = get_col, 
        group_indices = group_indices,
        count_unique = count_unique,
        as_breadth = as_breadth,
        pm_pbar=True, 
        pm_processes=cpus)
    X = pd.concat(x, axis = 1)
    return X

def tabify1_nr(query, filelist, on = 'vfamcdr3', get_col = "templates", sep = ",", 
    enforcement = True, cpus = 4, min_value = None,group_indices = None,
    count_unique = False, as_breadth = False):


    print(f"Tabulating {query.shape[0]} Inexact TCRs in {len(filelist)} files")
    x = parmap.map(tab1nr, filelist, 
        query = query[[on]], 
        on = on,
        sep = sep,
        min_value = min_value,
        enforcement = enforcement,
        get_col = get_col, 
        group_indices = group_indices,
        count_unique = count_unique,
        as_breadth = as_breadth,
        pm_pbar=True, 
        pm_processes=cpus)
    X = pd.concat(x, axis = 1)
    return X


def testify(binary_matrix, Dx, cpus =4, label = True, test = True, conditional_column = None):
    """
    Processes a binary matrix and computes statistical associations using Fisher's exact test.

    Parameters
    ----------
    binary_matrix : pd.DataFrame
        DataFrame representing a binary occurrence matrix.
    Dx : pd.DataFrame
        DataFrame containing additional data to analyze.
    cpus : int, optional
        Number of parallel processes to use (default is 4).
    label : bool, optional
        Whether to label the binary variables (default is True).
    test : bool, optional
        Whether to perform Fisher's exact tests (default is True).
    conditional_column : str or None, optional
        Column used for conditional filtering (default is None).

    Returns
    -------
    pd.DataFrame
        DataFrame containing computed association statistics.
    """
    assert isinstance(binary_matrix, pd.DataFrame)
    assert isinstance(Dx, pd.DataFrame)
    print(f"CHUNKING THE OCCURANCE MATRIX")
    nn_parts  = split_into_chunks(Dx, 1000)
    idx_parts = split_into_chunks(Dx.index.to_list(), 1000)
    two_part_iterable_input = [(idx, nnx) for idx, nnx in zip(idx_parts,nn_parts)]
    #k = iterable_func(x=two_part_iterable_input[0], binary_matrix =binary_matrix )
    print(f"TABULATING (A,B,C,D)")
    r = parmap.map(iterable_func, 
            two_part_iterable_input, 
            binary_matrix = binary_matrix,
            conditional_column = conditional_column,
            pm_processes=cpus, 
            pm_pbar = True)
    result = pd.concat(r)
    if label:
        print(f"LABELING BINARY VARIABLES")
        result['binary'] = result['m'].apply(lambda i: binary_matrix.columns[i])
    if test:
        print(f"FISHER'S EXACT TESTS")
        x1 = split_into_chunks(result, 100000)
        result = parmap.map(iterable_fisher, x1, pm_pbar = True, pm_processes = cpus)
        result = pd.concat(result)
    
    return result
    

