# import sys
# folder_path = '/fh/fast/gilbert_p/kmayerbl/tcrtest/'
# sys.path.append(folder_path)

import os
import subprocess
from os.path import join as opj

path_to_scripts = os.path.dirname(os.path.realpath(__file__))

MINICONDA_ENV   = "tcrdist311"
MINICONDA_ENV2  = "dask_env"
FINDER_SCRIPT   = os.path.join(path_to_scripts, "seqs_to_samples.py")
QUERY_SCRIPT    = "/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/nx.py"
BINTEST_SCRIPT  = "/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/public.py"
COMBINED_SCRIPT = "/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/nx_live.py"
GLIPH_SCRIPT    = "/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/gliph.py"
#MINICONDA_ENV = "dask_env"
# FINDER_SCRIPT = "/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/seqs_to_samples.py"


def run_anything(
    bash_path,
    infile,
    tasks = 2,
    mem = 16,
    time = "02:00:00",
    partition = "short",
    launch = False):
    """DEFAULT TEMPLATED FOR A SLURM RUNNER DOESNT DO ANYTHING """
    job_name    = f"{os.path.basename(infile)}"
    slurm_out   = opj(bash_path, f"{os.path.basename(infile)}_%j.out")
    slurm_error = opj(bash_path, f"{os.path.basename(infile)}_%j.err")
    sbatch_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --partition={partition}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={tasks}   
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_out}
#SBATCH --error={slurm_error}
source ~/.bashrc
conda activate {ANY_MINICONDA_ENV}
python {ANY_PTYTHON_SCRIPT} \\
    --bash_path={bash_path} \\
    --infile={infile}
"""
    bash_script_path = opj(bash_path, f"{job_name}.sh") 
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(sbatch_content)
    assert os.path.isfile(bash_script_path)
    print(sbatch_content)
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["chmod", "+x", bash_script_path])
        subprocess.run(["sbatch",      bash_script_path])
    return bash_script_path

def run_direct_finder(
    bash_path,
    infile,
    binary_matrix,
    binary_variables,
    outfile_binary_edit1,
    outfile_binary_edit0,
    sep,
    seqs_col,
    cpus,
    query_cdr3_col,
    query_v_col,
    query_j_col,
    query_vfam_col,
    sample_id_col,
    min_occur,
    min_collisions,
    min_pub,
    max_pval,
    max_pval_override0,
    max_pval_override1,
    allow_missing,
    tasks = 2,
    mem = 16,
    time = "02:00:00",
    partition = "short",
    launch = False, 
    setup_commands = None):

    job_name    = f"{os.path.basename(infile)}"
    slurm_out   = opj(bash_path, f"{os.path.basename(infile)}_%j.out")
    slurm_error = opj(bash_path, f"{os.path.basename(infile)}_%j.err")
    extra_options = ""
    if allow_missing:
        extra_options = extra_options +"--allow_missing"

    if setup_commands is None:
        setup_commands = f"source ~/.bashrc && conda activate {MINICONDA_ENV}" # this is what happens by default; may need to configure to your env

    sbatch_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --partition={partition}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={tasks}   
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_out}
#SBATCH --error={slurm_error}
{setup_commands}
python {FINDER_SCRIPT} \\
    --infile={infile} \\
    --binary_matrix={binary_matrix} \\
    --binary_variables={binary_variables} \\
    --outfile_binary_edit1={outfile_binary_edit1} \\
    --outfile_binary_edit0={outfile_binary_edit0} \\
    --sep={sep} \\
    --seqs_col={seqs_col} \\
    --cpus={cpus} \\
    --query_cdr3_col={query_cdr3_col} \\
    --query_v_col={query_v_col} \\
    --query_j_col={query_j_col} \\
    --query_vfam_col={query_vfam_col} \\
    --sample_id_col={sample_id_col} \\
    --min_occur={min_occur} \\
    --min_collisions={min_collisions} \\
    --min_pub={min_pub} \\
    --max_pval_override0 {max_pval_override0} \\
    --max_pval_override1 {max_pval_override1} \\
    --max_pval={max_pval} {extra_options}
"""
    bash_script_path = opj(bash_path, f"{job_name}.sh") 
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(sbatch_content)
    assert os.path.isfile(bash_script_path)
    print(sbatch_content)
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["chmod", "+x", bash_script_path])
        subprocess.run(["sbatch",      bash_script_path])
    return bash_script_path



def run_nx(bash_path,
              infile,
              outfile_csrmat_edit0,
              outfile_csrmat_edit1,
              tasks = 2,
              mem = 16,
              dask_report_name = "dask_report_name.html",
              time = "02:00:00",
              partition = "short",
              seqs_col = "amino_acid",
              sep = ",",
              min_occur = 1,
              uniquify = False,
              use_dask = False,
              launch = False):

    print(f"USING DASK: {use_dask}, TASKS: {tasks}")
    extra_options = ""
    if use_dask:
        extra_options = extra_options + " --use_dask"
        extra_options = extra_options + f" --dask_report_name {dask_report_name}"
        print(extra_options)
    job_name    = f"{os.path.basename(infile)}"
    slurm_out   = opj(bash_path, f"{os.path.basename(infile)}_%j.out")
    slurm_error = opj(bash_path, f"{os.path.basename(infile)}_%j.err")
    import math
    memory_limit = int(math.floor(mem/tasks))

    sbatch_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --partition={partition}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={tasks}   
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_out}
#SBATCH --error={slurm_error}
source ~/.bashrc
conda activate {MINICONDA_ENV}
python {QUERY_SCRIPT} \\
  --infile {infile} \\
  --sep {sep} \\
  --seqs_col {seqs_col} \\
  --outfile_csrmat_edit0 {outfile_csrmat_edit0} \\
  --outfile_csrmat_edit1 {outfile_csrmat_edit1} \\
  --min_occur {min_occur} \\
  --cpus {tasks} \\
  --memory_limit {memory_limit}GB {extra_options}
"""
    bash_script_path = opj(bash_path, f"{job_name}.sh") 
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(sbatch_content)
    assert os.path.isfile(bash_script_path)
    print(sbatch_content)
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["chmod", "+x", bash_script_path])
        subprocess.run(["sbatch",      bash_script_path])
    return bash_script_path

def run_binary_test(bash_path,
                    binary_matrix,
                    binary_variables,
                    csr_mat,
                    query_file,
                    subject_file, 
                    outfile,
                    tasks = 2,
                    mem = 16,
                    time = "02:00:00",
                    partition = "short",
                    query_cdr3_col = 'amino_acid',
                    query_v_col = 'v_gene',
                    query_j_col = 'j_gene',
                    sample_id_col = 'sample_id',
                    min_nn = 5, 
                    launch = False):
    infile = csr_mat
    extra_options = ""
    job_name    = f"{os.path.basename(infile)}"
    slurm_out   = opj(bash_path, f"BINTEST_{os.path.basename(infile)}_%j.out")
    slurm_error = opj(bash_path, f"BINTEST_{os.path.basename(infile)}_%j.err")
    
    sbatch_content = f"""#!/bin/bash
#SBATCH --job-name=BT_{job_name}
#SBATCH --partition={partition}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={tasks}   
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_out}
#SBATCH --error={slurm_error}
source ~/.bashrc
conda activate {MINICONDA_ENV2}
python {BINTEST_SCRIPT} \\
  --query_file {query_file} \
  --subject_file {subject_file} \
  --csr_mat {csr_mat} \
  --query_cdr3_col {query_cdr3_col} \
  --query_v_col {query_v_col} \
  --query_j_col {query_j_col} \
  --sample_id_col {sample_id_col} \
  --min_nn {min_nn} \
  --binary_matrix {binary_matrix} \
  --binary_variables {binary_variables} \
  --outfile {outfile} \
  --cpus {tasks}
"""
    
    bash_script_path = opj(bash_path, f"BINTEST_{job_name}.sh")
    
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(sbatch_content)
    assert os.path.isfile(bash_script_path)
    print(sbatch_content)
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["chmod", "+x", bash_script_path])
        subprocess.run(["sbatch",      bash_script_path])
    return bash_script_path


def run_gliph(bash_path,
                infile,
                stratv,
                outfile_csv_gliph,
                outfile_csrmat_gliph,
                outfile_csv_gliph_nr,
                outfile_csrmat_gliph_nr,
                infile_reference =  "/fh/fast/gilbert_p/kmayerbl/tcrtest_data/human.trb.strict_Vfam_200K.csv",
                ks = '7,8',
                left_trim = 3,
                right_trim = 2,
                fdr_max = 0.001,
                min_count = 5,
                tasks = 1,
                mem = 16,
                time = "02:00:00",
                partition = "short",
                launch = False):
    
    extra_options = ""
    job_name    = f"{os.path.basename(infile)}"
    slurm_out   = opj(bash_path, f"GLIPH_{os.path.basename(infile)}_%j.out")
    slurm_error = opj(bash_path, f"GLIPH_{os.path.basename(infile)}_%j.err")
    
    sbatch_content = f"""#!/bin/bash
#SBATCH --job-name=GLI_{job_name}
#SBATCH --partition={partition}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={tasks}   
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_out}
#SBATCH --error={slurm_error}
source ~/.bashrc
conda activate {MINICONDA_ENV2}
python {GLIPH_SCRIPT} \\
   --infile {infile} \\
   --stratv {stratv} \\
   --infile_reference {infile_reference} \\
   --outfile_csv_gliph {outfile_csv_gliph} \\
   --outfile_csrmat_gliph {outfile_csrmat_gliph} \\
   --outfile_csv_gliph_nr {outfile_csv_gliph_nr} \\
   --outfile_csrmat_gliph_nr {outfile_csrmat_gliph_nr} \\
   --ks {ks} \\
   --left_trim {left_trim} \\
   --right_trim {right_trim} \\
   --fdr_max {fdr_max} \\
   --min_count {min_count} 
"""
    
    bash_script_path = opj(bash_path, f"{job_name}_GLIPH.sh")
    
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(sbatch_content)
    assert os.path.isfile(bash_script_path)
    print(sbatch_content)
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["chmod", "+x", bash_script_path])
        subprocess.run(["sbatch",      bash_script_path])
    return bash_script_path





def run_nx_and_binary_test(bash_path,
              infile,
              outfile_csrmat_edit0,
              outfile_csrmat_edit1,
                binary_matrix,
                binary_variables,
                query_file,
                subject_file,         
                outfile_edit0, 
                outfile_edit1,
              tasks = 2,
              mem = 16,
              dask_report_name = "dask_report_name.html",
              time = "02:00:00",
              partition = "short",
              seqs_col = "amino_acid",
              sep = ",",
              min_occur = 1,
              uniquify = False,
              use_dask = False,
              min_nn = 5, 
                query_cdr3_col = 'amino_acid',
                query_v_col = 'v_gene',
                query_j_col = 'j_gene',
                sample_id_col = 'sample_id',
              launch = False):

    extra_options = ""
    if use_dask:
        extra_options = extra_options + " --use_dask"
        extra_options = extra_options + f" --dask_report_name {dask_report_name}"

    job_name    = f"{os.path.basename(infile)}"
    slurm_out   = opj(bash_path, f"{os.path.basename(infile)}_%j.out")
    slurm_error = opj(bash_path, f"{os.path.basename(infile)}_%j.err")
    import math
    memory_limit = int(math.floor(mem/tasks))

    sbatch_content = f"""#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --partition={partition}
#SBATCH --ntasks=1
#SBATCH --cpus-per-task={tasks}   
#SBATCH --mem={mem}G
#SBATCH --time={time}
#SBATCH --output={slurm_out}
#SBATCH --error={slurm_error}
source ~/.bashrc
conda activate {MINICONDA_ENV}
python {COMBINED_SCRIPT} \\
  --infile {infile} \\
  --sep {sep} \\
  --seqs_col {seqs_col} \\
  --outfile_csrmat_edit0 {outfile_csrmat_edit0} \\
  --outfile_csrmat_edit1 {outfile_csrmat_edit1} \\
  --min_occur {min_occur} \\
  --cpus {tasks} \\
  --memory_limit {memory_limit}GB \\
  --query_file {query_file} \\
  --subject_file {subject_file} \\
  --query_cdr3_col {query_cdr3_col} \\
  --query_v_col {query_v_col} \\
  --query_j_col {query_j_col} \\
  --sample_id_col {sample_id_col} \\
  --min_nn {min_nn} \\
  --binary_matrix {binary_matrix} \\
  --binary_variables {binary_variables} \\
  --outfile_edit0 {outfile_edit0} \\
  --outfile_edit1 {outfile_edit1} {extra_options}
"""
    bash_script_path = opj(bash_path, f"CX_{job_name}.sh") 
    print(f"Writing {bash_script_path}")
    with open(bash_script_path , 'w') as fh:
        fh.write(sbatch_content)
    assert os.path.isfile(bash_script_path)
    print(sbatch_content)
    if launch:
        print(f"Executing {bash_script_path}")
        subprocess.run(["chmod", "+x", bash_script_path])
        subprocess.run(["sbatch",      bash_script_path])
    return bash_script_path