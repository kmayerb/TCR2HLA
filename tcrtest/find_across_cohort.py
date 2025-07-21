# find_vcdr3_edit1
# feature_x_sample_binary_matrix, 
# feature_x_sample_frequency_matrix,
# feature_x_sample_templates_matrix,
# sum_unique_tcrs_detected_per_sample

import subprocess
import os
import sys
from scipy.sparse import load_npz
import numpy as np
import pandas as pd
from tqdm import tqdm
import parmap

def find_vfam_regex(
    bash_path,
    job_name,
    slurm_outname, #  opj(self.bash_path, f"{v}_HLAx%j.out"), 
    query_file, 
    subject_file, 
    csr_outfile,
    stratv_col = "v",
    csr_outfile_templates=None, 
    csr_outfile_freq=None,
    pattern_column='amino_acid',
    string_column='cdr3',
    freq_column='frequency',
    templates_column='templates',
    cpus=1,
    mem=16,
    time = "00:15:00",
    partition = 'short', 
    launch = True
    ):
    MINICONDA_ENV_VIZ = 'dask_env'
    FIND_VGLIPH_PATH   = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/bin/find_vfam_regex.py' 
    script_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={cpus}
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_outname}
#SBATCH --partition={partition}
source ~/.bashrc
conda activate {MINICONDA_ENV_VIZ}
python {FIND_VGLIPH_PATH} \
    --query_file {query_file} \\
    --subject_file {subject_file} \\
    --stratv_col {stratv_col} \\
    --csr_outfile {csr_outfile} \\
    --csr_outfile_templates {csr_outfile_templates} \\
    --csr_outfile_freq {csr_outfile_freq} \\
    --pattern_column {pattern_column} \\
    --string_column {string_column} \\
    --freq_column {freq_column} \\
    --templates_column {templates_column} 
"""
    print(script_content)
    bash_script_path = os.path.join(bash_path , f"{job_name}.find_vfam_gliph.bash")
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(script_content)
    assert os.path.isfile(bash_script_path)
    subprocess.run(["chmod", "+x", bash_script_path])
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["sbatch",      bash_script_path])
    return(bash_script_path)


def find_vfam_gliph(
    bash_path,
    job_name,
    slurm_outname, #  opj(self.bash_path, f"{v}_HLAx%j.out"), 
    query_file, 
    subject_file, 
    csr_outfile,
    stratv_col = "v",
    csr_outfile_templates=None, 
    csr_outfile_freq=None,
    pattern_column='amino_acid',
    string_column='cdr3',
    freq_column='frequency',
    templates_column='templates',
    cpus=1,
    mem=16,
    partition = 'short', 
    launch = True

    ):
    MINICONDA_ENV_VIZ = 'dask_env'
    FIND_VGLIPH_PATH   = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/bin/find_vfam_gliph.py' 
    script_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={cpus}
#SBATCH --mem={mem}G
#SBATCH --time=00:05:00
#SBATCH --output={slurm_outname}
#SBATCH --partition={partition}
source ~/.bashrc
conda activate {MINICONDA_ENV_VIZ}
python {FIND_VGLIPH_PATH} \
    --query_file {query_file} \\
    --subject_file {subject_file} \\
    --stratv_col {stratv_col} \\
    --csr_outfile {csr_outfile} \\
    --csr_outfile_templates {csr_outfile_templates} \\
    --csr_outfile_freq {csr_outfile_freq} \\
    --pattern_column {pattern_column} \\
    --string_column {string_column} \\
    --freq_column {freq_column} \\
    --templates_column {templates_column} 
"""
    print(script_content)
    bash_script_path = os.path.join(bash_path , f"{job_name}.find_vfam_gliph.bash")
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(script_content)
    assert os.path.isfile(bash_script_path)
    subprocess.run(["chmod", "+x", bash_script_path])
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["sbatch",      bash_script_path])
    return(bash_script_path)


def find_vcdr3_edit0(
    bash_path,
    job_name,
    slurm_outname, #  opj(self.bash_path, f"{v}_HLAx%j.out"), 
    query_file, 
    subject_file, 
    csr_outfile,
    csr_outfile_templates=None, 
    csr_outfile_freq=None,
    pattern_column='vfamcdr3',
    string_column='vfamcdr3',
    freq_column='freq',
    templates_column='templates',
    cpus=1,
    mem=8,
    partition = 'short', 
    launch = True):

    MINICONDA_ENV_VIZ = 'tcrdist311'
    FIND_VCDR3_PATH   = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/bin/find_vfamcdr3_edit0.py' 
    script_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={cpus}
#SBATCH --mem={mem}G
#SBATCH --time=00:05:00
#SBATCH --output={slurm_outname}
#SBATCH --partition={partition}
source ~/.bashrc
conda activate {MINICONDA_ENV_VIZ}
python {FIND_VCDR3_PATH} \
    --query_file {query_file} \\
    --subject_file {subject_file} \\
    --csr_outfile {csr_outfile} \\
    --csr_outfile_templates {csr_outfile_templates} \\
    --csr_outfile_freq {csr_outfile_freq} \\
    --pattern_column {pattern_column} \\
    --string_column {string_column} \\
    --freq_column {freq_column} \\
    --templates_column {templates_column} \\
    --cpus {cpus}
"""
    print(script_content)
    bash_script_path = os.path.join(bash_path , f"{job_name}.find_vcdr3_edit0.bash")
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(script_content)
    assert os.path.isfile(bash_script_path)
    subprocess.run(["chmod", "+x", bash_script_path])
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["sbatch",      bash_script_path])
    return(bash_script_path)


def find_vcdr3_edit1(
    bash_path,
    job_name,
    slurm_outname, #  opj(self.bash_path, f"{v}_HLAx%j.out"), 
    query_file, 
    subject_file, 
    csr_outfile,
    csr_outfile_templates=None, 
    csr_outfile_freq=None,
    pattern_column='vfamcdr3',
    string_column='vfamcdr3',
    freq_column='freq',
    templates_column='templates',
    cpus=1,
    mem=8,
    partition = 'short', 
    launch = True):

    MINICONDA_ENV_VIZ = 'tcrdist311'
    FIND_VCDR3_PATH   = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/bin/find_vfamcdr3_edit1.py' 
    script_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={cpus}
#SBATCH --mem={mem}G
#SBATCH --time=00:05:00
#SBATCH --output={slurm_outname}
#SBATCH --partition={partition}
source ~/.bashrc
conda activate {MINICONDA_ENV_VIZ}
python {FIND_VCDR3_PATH} \
    --query_file {query_file} \\
    --subject_file {subject_file} \\
    --csr_outfile {csr_outfile} \\
    --csr_outfile_templates {csr_outfile_templates} \\
    --csr_outfile_freq {csr_outfile_freq} \\
    --pattern_column {pattern_column} \\
    --string_column {string_column} \\
    --freq_column {freq_column} \\
    --templates_column {templates_column} \\
    --cpus {cpus}
"""
    print(script_content)
    bash_script_path = os.path.join(bash_path , f"{job_name}.find_vcdr3_edit1.bash")
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(script_content)
    assert os.path.isfile(bash_script_path)
    subprocess.run(["chmod", "+x", bash_script_path])
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["sbatch",      bash_script_path])
    return(bash_script_path)



def remove_low_values(x,min_value):
    """Here we let user ignore clones less than a certain value"""
    mask = x.copy()
    mask.data = np.where(mask.data >= min_value, 1, 0)
    x = x.multiply(mask)
    x.eliminate_zeros()
    return(x)


def non_duplicative_repertoire_summary(x, min_value=None, breadth = False, depth = False):
    if min_value is not None:
        mask = x.copy()
        mask.data = np.where(mask.data >= min_value, 1, 0)
        x = x.multiply(mask)
        x.eliminate_zeros()
    if breadth:
        breadth = (x > 0).max(axis =0).sum() / x.shape[1]
        return breadth
    if depth:
        depth = x.max(axis = 0).sum()
        return depth
    else: 
        return x


def feature_x_sample_frequency_matrix(fs, min_value = None):
    """
    Generate a frequency feature-by-sample matrix.

    Args:
        fs (list): A list of tuples containing sample names and corresponding npz file paths.

    Returns:
        pandas.DataFrame: DataFrame with sample ID named columns.
    """
    col_names =[i for i,_ in fs] 
    xs = list()
    print("LOADING .NPZ FILES")
    for _,f in tqdm(fs, total = len(fs)):
        xs.append(load_npz(f))
    if min_value is not None:

        F = np.array([np.array((remove_low_values(x,min_value).sum(axis =1)).astype(np.float32)).flatten() for x in xs]).\
            transpose()
    else:
        F = np.array([np.array((x.sum(axis =1)).astype(np.float32)).flatten() for x in xs]).\
            transpose()

    F = pd.DataFrame(F, columns = col_names)
    return(F)



def feature_x_sample_binary_matrix(fs,min_value = None):
    """
    Generate a binary feature-by-sample matrix.

    Args:
        fs (list): A list of tuples containing sample names and corresponding npz file paths.

    Returns:
        pandas.DataFrame: DataFrame with sample ID named columns.
    """
    col_names =[i for i,_ in fs] 
    xs = [load_npz(f) for _,f in fs]
    if min_value is not None:
        D = np.array([np.array((remove_low_values(x,min_value).sum(axis =1)).astype(np.int32)).flatten() for x in xs]).\
        transpose()
    else:
        D = np.array([np.array((x.sum(axis =1)).astype(np.int32)).flatten() for x in xs]).\
        transpose()
    D = pd.DataFrame(D, columns = col_names)
    return(D)



def feature_x_sample_templates_matrix(fs, min_value = None):
    """
    Generate a templates feature-by-sample matrix.

    Args:
        fs (list): A list of tuples containing sample names and corresponding npz file paths.

    Returns:
        pandas.DataFrame: DataFrame with sample ID named columns.
    """

    col_names =[i for i,_ in fs] 
    xs = list()
    print("LOADING .NPZ FILES")
    for _,f in tqdm(fs, total = len(fs)):
        xs.append(load_npz(f))

    if min_value is not None:
        T = np.array([np.array((remove_low_values(x, min_value).sum(axis =1)).astype(np.int32)).flatten() for x in xs]).\
        transpose()
    else:
        T = np.array([np.array((x.sum(axis =1)).astype(np.int32)).flatten() for x in xs]).\
        transpose()
    T = pd.DataFrame(T, columns = col_names)
    return(T)

def sum_unique_tcrs_detected_per_sample(fs, min_value):
    """
    Sum unique TCRs detected per sample across all features.

    Args:
        fs (list): A list of tuples containing sample names and corresponding npz file paths.

    Returns:
        pandas.Series: Series with sample names as index and the sum of unique TCRs detected per sample.
    """
    col_names = [i for i,_ in fs]  
    xs = [load_npz(f) for _,f in fs]
    if min_value is not None:
        UD = pd.Series(np.array([(remove_low_values(x, min_value).max(axis = 0)).sum() for x in xs]), index = col_names)
    else:
        UD = pd.Series(np.array([(x.max(axis = 0)).sum() for x in xs]), index = col_names)
    return(UD)

def get_sparse_matrix_shape(file_path):
    return load_npz(file_path).shape

def get_npz_shapes(file_paths, cpus=4):
    shapes = parmap.map(get_sparse_matrix_shape, file_paths, pm_processes=cpus, pm_pbar = True)
    return shapes

def unique_clone_counts_from_tuples(t, endswith ='.edit0.freq.npz', cpus = 24):
    
    fs_name = [x[0] for x in t]
    fs      = [x[1] for x in t]

    dims = get_npz_shapes(fs, cpus = 24)
    unique_clones= [x[1] for x in dims]
    
    df = pd.DataFrame({'sample_id':fs_name, 'unique_clones':unique_clones})
    df['log10unique'] = np.log10(df['unique_clones'])
    df.index = udf.sample_id
    return df

def unique_clone_counts_from_folder(r, endswith ='.edit0.freq.npz', cpus = 24):
    fs = os.listdir(r)
    fs = [f for f in fs if f.endswith(endswith)]
    fs_name = [os.path.basename(f).replace(endswith ,"") for f in fs]
    fs = [os.path.join(r,f) for f in fs]
    dims = get_npz_shapes(fs, cpus = 24)
    unique_clones= [x[1] for x in dims]
    df = pd.DataFrame({'sample_id':fs_name, 'unique_clones':unique_clones})
    df['log10unique'] = np.log10(df['unique_clones'])
    df.index = df.sample_id
    return df


def get_df_shape(file_path, sep = ","):
    return pd.read_csv(file_path, sep = sep).shape

def get_df_shapes(file_paths, sep = ",", cpus=4):
    shapes = parmap.map(get_df_shape, file_paths, sep = sep, pm_processes=cpus, pm_pbar = True)
    return shapes

def unique_clone_counts_from_folder_files(r, endswith ='.csv', sep = ",", cpus = 24):
    """ IF YOU WANT TO RIP SHAPE DIRECTLY FROM REPERTOIRE FILES """
    fs = os.listdir(r)
    fs = [f for f in fs if f.endswith(endswith)]
    fs_name = [os.path.basename(f).replace(endswith ,"") for f in fs]
    fs_name = [f.replace(".tsv" ,"") for f in fs_name]
    fs = [os.path.join(r,f) for f in fs]
    dims = get_df_shapes(fs, sep = sep, cpus = cpus)
    unique_clones= [x[0] for x in dims]
    df = pd.DataFrame({'sample_id':fs_name, 'unique_clones':unique_clones})
    df['log10unique'] = np.log10(df['unique_clones'])
    df.index = df.sample_id
    return df




def load_and_process_npz(file_path, min_value=None):
    """Load and process a single .npz file."""
    x = load_npz(file_path)
    if min_value is not None:
        return np.array(remove_low_values(x, min_value).sum(axis=1)).astype(np.float32).flatten()
    else:
        return np.array(x.sum(axis=1)).astype(np.float32).flatten()

def feature_x_sample_frequency_matrix_parmap(fs, min_value=None, cpus = 24):
    """
    Generate a frequency feature-by-sample matrix.

    Args:
        fs (list): A list of tuples containing sample names and corresponding .npz file paths.

    Returns:
        pandas.DataFrame: DataFrame with sample ID named columns.
    """
    col_names = [i for i, _ in fs]
    file_paths = [f for _, f in fs]

    print("LOADING AND PROCESSING .NPZ FILES IN PARALLEL")
    xs = parmap.map(load_and_process_npz, file_paths, min_value=min_value, pm_pbar=True, pm_processes = cpus)

    F = np.array(xs).transpose()
    F = pd.DataFrame(F, columns=col_names)
    return F


# USE THIS IF YOU WANT TO COUNT OCCURANCES OF A FEATURE WITH ABILITY TO CHECK FREQUENCY.
def load_and_process_frequency_detect_npz(file_path, min_value=None):
    """Load and process a single .npz file."""
    x = load_npz(file_path)
    if min_value is not None:
        return np.array((remove_low_values(x, min_value) > 0).sum(axis=1)).astype(np.float32).flatten()
    else:
        return np.array((x > 0).sum(axis=1)).astype(np.float32).flatten()

def feature_x_sample_frequency_detect_matrix_parmap(fs, min_value=None, cpus = 24):
    """
    Generate a frequency feature-by-sample matrix.

    Args:
        fs (list): A list of tuples containing sample names and corresponding .npz file paths.

    Returns:
        pandas.DataFrame: DataFrame with sample ID named columns.
    """
    col_names = [i for i, _ in fs]
    file_paths = [f for _, f in fs]

    print("LOADING AND PROCESSING .NPZ FILES IN PARALLEL")
    xs = parmap.map(load_and_process_frequency_detect_npz, file_paths, min_value=min_value, pm_pbar=True, pm_processes = cpus)

    F = np.array(xs).transpose()
    F = pd.DataFrame(F, columns=col_names)
    return F


from scipy.sparse import load_npz
def non_duplicative_repertoire_summary_indices(x, query, group_indices, min_value=None, breadth = False, depth = False):
    name = x[0]
    x = load_npz(x[1])
    #print(name, x.shape)
    import numpy as np
    # THIS ALLOWS FOR DROPPING LOW FREQUENCY CLONES
    if min_value is not None:
        mask = x.copy()
        mask.data = np.where(mask.data >= min_value, 1, 0)
        x = x.multiply(mask)
        x.eliminate_zeros()

    df = query.copy()
    df['group_indices'] = group_indices
    store = dict()
    for i,g in df.groupby(group_indices):
        g_ix = g.index
        x_subset = x[g_ix]

        if breadth:
            store[i]=(x_subset > 0).max(axis =0).sum() / x.shape[1]
        elif depth:
            store[i]=x_subset.max(axis = 0).sum()
        else: 
            raise ValueError("breadth or depth must be True")
    return pd.Series(store, name = name)

def nr_summary_parmap(file_paths, query, group_indices, min_value=None,breadth = False, depth = False, cpus = 4):
    import parmap
    xs = parmap.map(non_duplicative_repertoire_summary_indices, 
        file_paths, 
        query=query, 
        group_indices= group_indices,
        min_value=min_value, 
        breadth = breadth, 
        depth = depth,
        pm_pbar=True, 
        pm_processes = cpus)
    return pd.concat(xs, axis = 1)



