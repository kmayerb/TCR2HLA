# seqs_to_samples
# import sys
# folder_path = '/fh/fast/gilbert_p/kmayerbl/tcrtest/'
# sys.path.append(folder_path)
import sys
try:
    import tcrtest
except ModuleNotFoundError as e:
    print(
        "\n[WARNING] Could not import tcrtest modules. "
        "This usually means you are running locally without installing the package.\n"
        "Temporarily adding '/fh/fast/gilbert_p/kmayerbl/TCR2HLA' to your Python path.\n"
        f"Original error: {e}\n"
    )
    sys.path.insert(0, "/fh/fast/gilbert_p/kmayerbl/TCR2HLA")
    import tcrtest

import argparse
import os
import psutil
import numpy as np 
import pandas as pd
import time
from tqdm import tqdm

from collections import Counter
from datetime import datetime
from scipy.sparse import csr_matrix, save_npz

from tcrtest.permutation import get_permutations, get_identity
from tcrtest.testing import split_into_chunks, matrix_tab, matrix_tab_w_missing_info # new feature
from tcrtest.collision import collision, get_unique_collisions, get_unique_collisions_one_cpu
from tcrtest.nx import  seqs_to_ix


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
def iterable_hla_function2(x, 
    binary_matrix, 
    conditional_column= None, 
    subset_column = None):
    idx = x[0]
    nnx = x[1]
    p = np.array([np.vstack(x) for x in nnx])
    result = matrix_tab(M = binary_matrix.values, 
        p = p, 
        idx = idx, 
        conditional_column= conditional_column, 
        subset_column = subset_column)
    return(result)
# this is a new feature.
def iterable_hla_function3(x, 
    binary_matrix, 
    conditional_column= None, 
    subset_column = None):
    idx = x[0]
    nnx = x[1]
    # No need to vstack with new function
    #p = np.array([np.vstack(x) for x in nnx])
    p = nnx 
    result = matrix_tab_w_missing_info(M = binary_matrix.values, 
        p = p, 
        idx = idx, 
        conditional_column= conditional_column, 
        subset_column = subset_column)
    return(result)

def iterable_fisher(x1):
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
    x1 = x1.sort_values('p_exact').reset_index(drop = True)
    return(x1)
def yield_uj(df, col = 'i'):
    grouped = df.groupby(col)
    for i, group in grouped:
        yield (i, group['uj'].values)   

def map_ij_to_samples(data, ij_u, binary_matrix, seqs, seqs_u_inv, min_pub , args):
    
    i_to_sample = {i:s for i,s in zip(data.index, data.sample_id)}
    i_to_u = {i:u for i,u in enumerate(seqs_u_inv)}
    # last i for each unique
    u_to_i = {u:i for i,u in i_to_u.items()}
    i_to_s = {i:s for i,s in zip(data.index, data['sample_id'])}
    u_to_s = {i_to_u.get(i):s for i,s in zip(data.index, data['sample_id'])}

    # We need a tool for table joins unique values to original indices 
    d_i = pd.DataFrame({'ui':list(range(seqs.shape[0])) }, dtype = 'uint32')
    d_j = pd.DataFrame({'uj':list(range(seqs.shape[0])) }, dtype = 'uint32')
    d_i['i'] = d_i['ui'].apply(lambda x: i_to_u.get(x))
    d_j['j'] = d_j['uj'].apply(lambda x: i_to_u.get(x))

    iterations = ij_u['i'].nunique()
    pps = list()
    ix = list()
    print(f"{clock()} -- Mapping sequence to sample_id matrix")
    for i, nn in tqdm(yield_uj(ij_u), total = iterations) :
        x = data['sample_id'].iloc[nn]
        pp = binary_matrix.index.isin(x).astype('uint8')
        pps.append(pp)
        ix.append(i)
    X=np.vstack(pps)

    index_to_search= ij_u.groupby('i').\
        head(1).\
        merge(d_i).\
        groupby('i').\
        head(1)['ui']

    assert(len(index_to_search) == X.shape[0])

    # only consider things that are readonably public
    pub_ix = X.sum(axis = 1) > min_pub
    if pub_ix.sum() == 0:
        print(f"{clock()} -- No public TCRs, returning None")
        return None

    Xpub = X[pub_ix,:]
    index_to_search_pub = index_to_search[pub_ix]
    assert(len(index_to_search_pub) == Xpub.shape[0])

    memory_usage = get_memory_usage()
    print(f"{clock()} -- Current memory usage: {memory_usage / (1024 ** 2):.2f} MB")

    csr_mat = csr_matrix(Xpub)
    csr_mat_columns = binary_matrix.index
    feature_data = data.loc[index_to_search_pub,][[ args.query_vfam_col,'v_gene','j_gene','amino_acid']]

    print(f"{clock()} -- Tabulating against binary matrix: {memory_usage / (1024 ** 2):.2f} MB")
    nn_parts  = split_into_chunks(Xpub, 1000)
    idx_parts = split_into_chunks(list(index_to_search_pub), 1000)
    assert(len(nn_parts) == len(idx_parts))
    for i in range(len(nn_parts)):
        assert nn_parts[i].shape[0] == len(idx_parts[i])

    if args.cpus == 1:
        r = list()
        for i in tqdm(range(len(nn_parts)), total = len(nn_parts)):
            p = np.array([np.vstack(x) for x in nn_parts[i]])
            idx = list(idx_parts[i])
            result = matrix_tab(M = binary_matrix.values, 
                p = p, 
                idx = idx, 
                conditional_column= None, 
                subset_column = None)
            r.append(result)
    else:
        import parmap
        two_part_iterable_input = [(idx, nnx) for idx, nnx in zip(idx_parts,nn_parts)]
        r = list()
        #import pdb; pdb.set_trace()
        
        if np.any(binary_matrix.isna()):
            """if there is any uncertainty or missingness in the binary matrix"""
            # can handle NAs
            r = parmap.map(iterable_hla_function3, 
                    two_part_iterable_input, 
                    binary_matrix,
                    pm_processes=args.cpus, 
                    pm_pbar = True)
        else:
            # original can not handle NAs
            r = parmap.map(iterable_hla_function2, 
                    two_part_iterable_input, 
                    binary_matrix,
                    pm_processes=args.cpus, 
                    pm_pbar = True)

    
    memory_usage = get_memory_usage()
    print(f"{clock()} -- Current memory usage: {memory_usage / (1024 ** 2):.2f} MB")
    #import pdb; pdb.set_trace()
    x1 = pd.concat(r)


    memory_usage = get_memory_usage()
    print(f"{clock()} -- Current memory usage: {memory_usage / (1024 ** 2):.2f} MB")

    print(f"{clock()} -- Performing Fishers Exact Tests")
    x1 = split_into_chunks(x1, 100000)
   
    if args.cpus == 1:
        store = list()
        for x in x1:
            store.append(iterable_fisher(x))
        x1 = store
   
    else:
        x1 = parmap.map(iterable_fisher, x1, pm_pbar = True, pm_processes = args.cpus)
   
    x1 = pd.concat(x1)
   
    print(f"{clock()} -- Populating results with search sequence details")
    tmp = x1[x1['p_exact'] < args.max_pval].reset_index(drop = True).sort_values('p_exact')
    tmp['binary'] = tmp['m'].apply(lambda i : binary_matrix.columns[i])
    query = feature_data
    tmp['cdr3']   = query[args.query_cdr3_col].loc[tmp['i']].to_list()
    tmp['v_gene'] = query[args.query_v_col].loc[tmp['i']].to_list()
    tmp['j_gene'] = query[args.query_j_col].loc[tmp['i']].to_list()
    tmp['V'] = query[args.query_vfam_col].loc[tmp['i']].to_list()
    
    #print(tmp.query('binary == "A_0201"').head(30))
    return tmp


def main(data, args):

    binary_matrix =  pd.read_csv(args.binary_matrix,
        sep = "\t",
        index_col = 0)

     
    # Perform overlap check
    print(f"{clock()} -- Setting binary matrix to specified variable")
    bin_vars_all = args.binary_variables.split(",")
    bin_vars = [x for x in args.binary_variables.split(",") if x in binary_matrix.columns]
    missing_bin_vars = list(set(bin_vars_all) - set(bin_vars))
    if len(missing_bin_vars) > 0:
        print(f"\tWARNING: Not all variables found")
        for i in missing_bin_vars:
            print(f"\t{i} was missing in {args.binary_matrix}")
    if len(bin_vars) == 0:
        raise ValueError("none of binary variable were present in binary matrix")
    binary_matrix = binary_matrix[bin_vars]
    
    # Default is two check for missing, but we can disable this
    if args.allow_missing == False:
        if binary_matrix.isna().sum().sum() != 0:
            print(f"\tWARNING: Can't handle NAs in the binary matrix")
            bin_vars_with_nas = binary_matrix.isna().sum()[binary_matrix.isna().sum() > 0].index
            for b in bin_vars_with_nas:
                print(f"\t{b} was removed due to presence of NA values")
            bin_vars = [ i for i in bin_vars if i not in bin_vars_with_nas]

    binary_matrix = binary_matrix[bin_vars]
    



    # THIS BLOCK JUST PERFORMS A CHECK OF CONSITENCY BETWEEN 
    # SAMPLE VECTOR IN THE INFILE DATA AND THE BINARY MATRIX FILE

    sample_vector = data[args.sample_id_col]
    print(f"{clock()} -- Checking sample_id against binary_matrix index")
    all_samples = set(sample_vector)
    matrix_samples = set(binary_matrix.index)
    missing_sample_subject = all_samples - matrix_samples
    missing_sample_binary = matrix_samples-all_samples
    overlaping_samples = matrix_samples.intersection(all_samples)
    if len(missing_sample_subject) > 0:
        print(f"\tWARNING: Not all samples in subject or query file were found in binary matrix")
    missing_sample_subject = all_samples - matrix_samples
    if len(missing_sample_subject) > 0:
        n = len(missing_sample_subject)
        print(f"\tWARNING: Some sample (n = {n}) in sequences file were not found in binary matrix. These simply won't be counted in the association testing")
    if len(missing_sample_binary) > 0:
        n = len(missing_sample_binary)
        print(f"\tWARNING: Some sample names (n = {n}) in binary matrix index did not appear in sequences file, This will distort statitics")
    if len(overlaping_samples) == 0:
        raise ValueError("None of sample_id match those in binary matrix index. Recheck inputs")

    seqs = data[args.seqs_col].values
    n_seqs = seqs.shape[0]
    x, seqs_u, seqs_u_inv,  n_row_u,  n_col_u  = seqs_to_ix(seqs, cpus =args.cpus, collision = collision)
    # Mappers
        # i - index is unique set
        # u - index in original set
        # s - sample
    i_to_sample = {i:s for i,s in zip(data.index, data.sample_id)}
    i_to_u = {i:u for i,u in enumerate(seqs_u_inv)}
    # last i for each unique
    u_to_i = {u:i for i,u in i_to_u.items()}
    i_to_s = {i:s for i,s in zip(data.index, data['sample_id'])}
    u_to_s = {i_to_u.get(i):s for i,s in zip(data.index, data['sample_id'])}

    # We need a tool for table joins unique values to original indices 
    d_i = pd.DataFrame({'ui':list(range(seqs.shape[0])) }, dtype = 'uint32')
    d_j = pd.DataFrame({'uj':list(range(seqs.shape[0])) }, dtype = 'uint32')
    d_i['i'] = d_i['ui'].apply(lambda x: i_to_u.get(x))
    d_j['j'] = d_j['uj'].apply(lambda x: i_to_u.get(x))

    # Compute permutations on unique sequences
    print(f"{clock()} -- Getting edges by permutations")
    t0 = time.perf_counter()
    ij_u = get_permutations(x)
    ii_u = get_identity(n_row_u)
    ij_u = pd.concat([ii_u, ij_u])
    memory_usage = get_memory_usage()
    print(f"{clock()} -- Current memory usage: {memory_usage / (1024 ** 2):.2f} MB")
    ij_u = ij_u.drop_duplicates()
    ij_u = ij_u.sort_values(['i','j'])
    ij_u = ij_u.merge(d_j)


    print(f"{clock()} -- Removing sequences with less than {args.min_collisions +1} neighbors")
    # remove sequences that don't have any collisions
    print(ij_u.shape)
    ij_u = ij_u.groupby('i').filter(lambda x: len(x) > args.min_collisions)#5)
    ij_u = ij_u.sort_values(['i','j'])
    print(ij_u.shape)
    #import pdb; pdb.set_trace()
    min_pub1 = args.min_pub
    #import pdb; pdb.set_trace()
    tmp1 = map_ij_to_samples(data, ij_u, binary_matrix, seqs, seqs_u_inv, min_pub1, args)
    
    # WE CAN DO THIS BASED ON EXACT MATCH NOW:
    ij_u = ij_u[ij_u['i']==ij_u['j']]
    print(f"{clock()} -- Removing sequences with less than 2 neighbors")
    # remove sequences that don't have any collisions
    ij_u = ij_u.groupby('i').filter(lambda x: len(x) > 1)
    ij_u = ij_u.sort_values(['i','j'])
    min_pub0 = 1
    if ij_u.shape[0] == 0:
        print(f"{clock()} -- No sequences left")
        tmp0 = None 

    else:
        tmp0 = map_ij_to_samples(data, ij_u, binary_matrix, seqs, seqs_u_inv, min_pub0, args)
    return tmp0, tmp1




if __name__ == "__main__":

    # Required positional argument
    parser = argparse.ArgumentParser(description='Process the command line arguments.')
    parser.add_argument('--infile', type=str, required=True, help='The path to the input file')
    parser.add_argument('--binary_matrix', type=str, required=True, help='Path to the binary matrix file')
    parser.add_argument('--binary_variables', type=str, required=True, help='Binary variables to use')
    parser.add_argument('--outfile_binary_edit1', type=str, required=True, help='Output file path edit 1')
    parser.add_argument('--outfile_binary_edit0', type=str, required=True, help='Output file path edit 0')


    parser.add_argument('--sep', type=str, default=",", help='sep for infile (default .csv)')
    parser.add_argument('--seqs_col', type=str, default="amino_acid", help='Column name to use (default: amino_acid)')
    parser.add_argument('--cpus', type=int, default=2, help='Number of CPUs to use (default: 20)')

    parser.add_argument('--query_cdr3_col', type=str, default = 'amino_acid', help='Column name for V gene')
    parser.add_argument('--query_v_col', type=str, default = 'v_gene', help='Column name for V gene')
    parser.add_argument('--query_j_col', type=str, default = 'j_gene', help='Column name for J gene')
    parser.add_argument('--query_vfam_col', type=str, default = 'v', help='Column name for VFam')
    parser.add_argument('--sample_id_col', type=str, default= 'sample_id', help='Column name for sample ID')


    parser.add_argument('--min_occur', type = int, default = 1, help = 'refers to minium collision key occurance')
    parser.add_argument('--min_collisions', type = int, default = 5, help = 'refers to minimum total neighbors collided')
    #parser.add_argument('--min_nn', type=int, default=5,        help = 'refers to minimum total neighbors collided')
    parser.add_argument('--min_pub', type = int, default = 5,   help = 'refers to minium publicity of a feature considering all collisions')
    parser.add_argument('--max_pval', type = float, default = 1e-1,   help = 'refers to minium publicity of a feature considering all collisions')
    parser.add_argument('--max_pval_override1', type = float, default = 1e-3,   help = 'refers to minium publicity of a feature considering all collisions')
    parser.add_argument('--max_pval_override0', type = float, default = 1e-1,   help = 'refers to minium publicity of a feature considering all collisions')
    #parser.add_argument('--max_pval0', type = float, default = 5e-2,   help = 'refers to minium publicity of a feature considering all collisions')
    parser.add_argument('--allow_missing', action = "store_true")
    """
    CPUS = 10
    INFILE='~/V19_100K_test.csv'
    BINARY_MATRIX='/fh/fast/gilbert_p/kmayerbl/tcr_utils/data/Emerson_newest_hla_info_df.tsv_hla_boolean_SUBSET_TO_NO_NAs.tsv'
    BINARY_VARIABLES='A_0201,A_0101,A_0301,A_2402,A_1101,A_2902,A_2601,A_3201,A_6801,A_3101,A_2501,B_0702,B_0801,B_4402,B_1501,B_3501,B_5101,B_4403,B_1801,B_4001,B_2705,B_5701,B_1402,B_1302,B_3801,C_0701,C_0702,C_0401,C_0501,C_0602,C_0304,C_1203,C_0303,C_0102,C_0802,C_0202,C_1601,C_1402,DQB1_0301,DQB1_0602,DQB1_0201,DQB1_0501,DQB1_0202,DQB1_0302,DQB1_0603,DQB1_0303,DQB1_0604,DQB1_0503,DQB1_0402,DRB1_0301,DRB1_0701,DRB1_1501,DRB1_0401,DRB1_0101,DRB1_1101,DRB1_1301,DRB1_1302,DRB1_0404,DRB1_1104,DRB1_0102,DQA1_0102__DQB1_0602,DQA1_0102__DQB1_0201,DQA1_0102__DQB1_0202,DQA1_0102__DQB1_0604,DQA1_0201__DQB1_0602,DQA1_0201__DQB1_0202,DQA1_0201__DQB1_0303,DQA1_0501__DQB1_0602,DQA1_0501__DQB1_0201,DQA1_0103__DQB1_0603,DQA1_0401__DQB1_0402,DQA1_0104__DQB1_0503'
    OUTFILE_BINARY_EDIT1='~/V19_100K_test.edit1.fts.csv'
    OUTFILE_BINARY_EDIT0='~/V19_100K_test.edti0.fts.csv'

    python /fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/seqs_to_samples.py \
         --infile $INFILE \
         --cpus $CPUS \
         --binary_matrix $BINARY_MATRIX \
         --binary_variables $BINARY_VARIABLES \
         --outfile_binary_edit1 $OUTFILE_BINARY_EDIT1 \
         --outfile_binary_edit0 $OUTFILE_BINARY_EDIT0
    """

    args = parser.parse_args()
    args.bin_vars = args.binary_variables
    
    data =  pd.read_csv(args.infile, sep = args.sep)
    tmp0, tmp1 = main(data, args)
    
    print(f"{clock()} -- Writing {args.outfile_binary_edit1}")
    print(tmp1)
    
    if args.max_pval_override1 is not None:
        tmp1 = tmp1[tmp1['p_exact'] < args.max_pval_override1].reset_index(drop = True)
    tmp1.to_csv(args.outfile_binary_edit1, index = False)


    print(f"{clock()} -- Writing {args.outfile_binary_edit0}")
    print(tmp0)
    if args.max_pval_override0 is not None:
        tmp0 = tmp0[tmp0['p_exact'] < args.max_pval_override0].reset_index(drop = True)
    tmp0.to_csv(args.outfile_binary_edit0, index = False)
    


    # parser.add_argument('--outfile_csrmat_edit0', type=str, default='test.edit0.npz', help='Output file name for csr matrix edit 0')
    # parser.add_argument('--outfile_csrmat_edit1', type=str, default='test.edit1.npz', help='Output file name for csr matrix edit 1')



    # args = argparse.Namespace()
    # args.cpus = 12
    # args.infile = '/fh/scratch/delete90/gilbert_p/TCRTEST_CL_EMERSON_V1/stratv_combined/V05_combined.csv'
    # args.binary_matrix = '/fh/fast/gilbert_p/kmayerbl/tcr_utils/data/Emerson_newest_hla_info_df.tsv_hla_boolean_SUBSET_TO_NO_NAs.tsv'
    # args.min_occur = 1
    # args.min_nn = 5
    # args.min_pub = 5
    # args.seqs_col = 'amino_acid'
    # args.sample_id_col = 'sample_id'
    # args.min_collisions = 5
    # args.bin_vars = 'A_0201,A_0101,A_0301,A_2402,A_1101,A_2902,A_2601,A_3201,A_6801,A_3101,A_2501,B_0702,B_0801,B_4402,B_1501,B_3501,B_5101,B_4403,B_1801,B_4001,B_2705,B_5701,B_1402,B_1302,B_3801,C_0701,C_0702,C_0401,C_0501,C_0602,C_0304,C_1203,C_0303,C_0102,C_0802,C_0202,C_1601,C_1402,DQB1_0301,DQB1_0602,DQB1_0201,DQB1_0501,DQB1_0202,DQB1_0302,DQB1_0603,DQB1_0303,DQB1_0604,DQB1_0503,DQB1_0402,DRB1_0301,DRB1_0701,DRB1_1501,DRB1_0401,DRB1_0101,DRB1_1101,DRB1_1301,DRB1_1302,DRB1_0404,DRB1_1104,DRB1_0102,DQA1_0102__DQB1_0602,DQA1_0102__DQB1_0201,DQA1_0102__DQB1_0202,DQA1_0102__DQB1_0604,DQA1_0201__DQB1_0602,DQA1_0201__DQB1_0202,DQA1_0201__DQB1_0303,DQA1_0501__DQB1_0602,DQA1_0501__DQB1_0201,DQA1_0103__DQB1_0603,DQA1_0401__DQB1_0402,DQA1_0104__DQB1_0503'
    # args.query_cdr3_col = 'amino_acid'
    # args.query_v_col = 'v_gene'
    # args.query_j_col = 'j_gene'
    # args.query_vfam_col = 'v'

    # x4   = pd.read_csv(args.infile)
    # x3   = pd.read_csv('/fh/scratch/delete90/gilbert_p/TCRTEST_CL_EMERSON_V1/stratv_combined/V19_combined.csv')
    # data = x4 #x4.sample(15_000_000).reset_index(drop = True)
    # #tmp = main(data, args)
    # tmp0, tmp1 = main(data, args)


    # for v in ['V19','V27','V30','V28','V07','V20','V06',
    #     'V02','V18','V25','V12','V04','V29',
    #     'V14','V13','V11','V16','V10','V15','V03','V09']:
    #     print(v)
    #     v ='V19'
    #     x5 = pd.read_csv(f'/fh/scratch/delete90/gilbert_p/TCRTEST_CL_EMERSON_V1/stratv_combined/{v}_combined.csv')
    #     data = x5.sample(5_000_000).reset_index(drop = True)
    #     tmp0, tmp1 = main(data, args)
    #     tmp1.to_csv(os.path.join('/fh/scratch/delete90/gilbert_p/TCRTEST_CL_EMERSON_V1/SEQ_TO_SAMPLE/', f"{v}_seq_to_sample.edit1.csv"), index = False)
    #     tmp0.to_csv(os.path.join('/fh/scratch/delete90/gilbert_p/TCRTEST_CL_EMERSON_V1/SEQ_TO_SAMPLE/', f"{v}_seq_to_sample.edit0.csv"), index = False)




