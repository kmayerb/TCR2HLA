# ui.py
# import sys
# folder_path = '/fh/fast/gilbert_p/kmayerbl/tcrtest/'
# sys.path.append(folder_path)

from os.path import isdir, isfile
from os.path import join as opj
from scipy.sparse import load_npz, save_npz
import pandas as pd 
import numpy as np
import os
import parmap
import subprocess
import zipfile
from progress.bar import Bar
import math 
import re
from functools import partial

from tcrtest.run import run_nx
from tcrtest.run import run_direct_finder, run_nx, run_binary_test, run_gliph #run_nx_and_binary_test
from tcrtest.parse import parse_v2x_gt1, parse_v2x_gt1_vfamcdr3

class VfamCDR3():
    def __init__(self    ,
        project_folder   ,
        input_zfile      = None,  
        input_gz_file      = None,
        outdir_stratv    = None,
        outdir_stratv_combined = None,
        query_folder     = None,
        subject_folder   = None,
        cpus = 1, 
        mode = 'slurm'):

        self.cpus = cpus
        self.mode = mode
        self.project_folder = project_folder
        self.input_zfile    = input_zfile 
        self.input_gz_file  = input_gz_file 
        self.outdir         = project_folder
        self.bash_path = os.path.join(self.project_folder, "bash")
        self.launch_paths = dict()
        self.subject_folder = subject_folder
        self.vlist =['V01', 'V02', 'V03', 'V04', 'V05', 'V06', 'V07','V08', 'V09', 'V10', 
                     'V11', 'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V18', 'V19','V20', 
                     'V21', 'V22', 'V23', 'V24', 'V25', 'V26','V27', 'V28', 'V29', 'V30',
                     'V31','V32','V33','V34','V35','V36','V37','V38','V39','V40','V41','VA']

        self.init_folders()

    def init_folders(self):
        self.outdir_stratv  = os.path.join(self.outdir, "stratv")
        self.outdir_stratv_combined = os.path.join(self.outdir, "stratv_combined")
        self.outdir_stratv_npz = os.path.join(self.outdir, "stratv_npz")
        self.outdir_stratv_hla = os.path.join(self.outdir, "stratv_hla")
        self.outdir_hla_clones = os.path.join(self.outdir, "hla_clones")
        self.outdir_vfamcdr3 = os.path.join(self.outdir, "vfamcdr3")
        self.outdir_vfamcdr3_subsample = os.path.join(self.outdir, "vfamcdr3_subsample")
        self.bash_path = os.path.join(self.project_folder, "bash")

        if_not_create_dir(self.outdir)
        if_not_create_dir(self.outdir_stratv)
        if_not_create_dir(self.outdir_stratv_combined)
        if_not_create_dir(self.outdir_stratv_npz)
        if_not_create_dir(self.outdir_stratv_hla)
        if_not_create_dir(self.outdir_hla_clones)
        if_not_create_dir(self.outdir_vfamcdr3)
        if_not_create_dir(self.bash_path)
        if_not_create_dir(self.outdir_vfamcdr3_subsample)
        for v in self.vlist:
            dest_v= os.path.join(self.outdir_stratv,v)
            if_not_create_dir(dest_v)
    
    def list_raw_files(self, n = 5):
        z = zipfile.ZipFile(os.path.join(self.input_zfile))
        z_fs = [x for x in z.namelist() if x.endswith("tsv") or x.endswith("csv")]
        print(f"Raw data contains ({len(z_fs)}) files, showing first {n}")
        for i in range(n):
            f = z_fs[i]
            print(f"\t {i} -- {f}")
    
    def get_raw_files(self):
        z = zipfile.ZipFile(os.path.join(self.input_zfile))
        z_fs = [x for x in z.namelist() if x.endswith("tsv") or x.endswith("csv")]
        return(z_fs)
    
    def parse_adaptive_files(self, checklist = None):
        """
        Convenience File For Default Behavior 
        """
        self.vfamcdr3_v2_parmap(parse_func = parse_v2x_gt1_vfamcdr3, checklist = checklist)

    def stratify_adaptive_files(self, checklist = None):
        """
        Convenience File For Default Behavior 
        """
        self.stratv_v2_parmap(parse_func = parse_v2x_gt1,checklist = checklist)

    def stratv_from_file(self, input_file = None, stratify_col = 'v'):
        if input_file is None:
            input_file = self.input_gz_file
            assert input_file is not None, "You did not provide a .gz file"
        df = pd.read_csv(input_file)
        with Bar('Processing', max=len(self.vlist)) as bar:
            for v,dfv in df.groupby(stratify_col):
                print(f" For -- {v}, Writing:")
                print(os.path.join(self.outdir_stratv_combined, f"{v}_combined.csv"))
                ix1 = dfv['amino_acid'].str.len() >= 6
                ix2 = dfv['amino_acid'].notna()
                dfv = dfv[ix1&ix2].reset_index(drop = True)
                dfv.to_csv(os.path.join(self.outdir_stratv_combined, f"{v}_combined.csv"), 
                    index = False)
                bar.next()

    def stratv_v2_parmap(self, input_file = None, parse_func = None, checklist = None, sep = "\t"):
        if input_file is None:
            input_file = self.input_zfile
            assert input_file is not None, "You did not provide a .zipfile file"
        z = zipfile.ZipFile(os.path.join(self.input_zfile))
        z_fs = [x for x in z.namelist() if x.endswith("tsv") or x.endswith('csv')]
        if checklist is not None:
            z_fs = [x for x in z_fs if x in checklist]
            print(z_fs)
            print(f"Length of {len(z_fs)} after using checklist")
        print(f"Stratifying files; Accelerating with parmap and {self.cpus} cpus")
        if parse_func is not None:
            strat_v2_single_ = partial(strat_v2_single, parse_func= parse_func, sep = sep)
        else:
            strat_v2_single_ = strat_v2_single
        parmap.map(strat_v2_single_, z_fs, 
            zfile =self.input_zfile, 
            outdir =self.outdir_stratv, 
            pm_pbar = True, pm_processes = self.cpus)

    def vfamcdr3_v2_parmap(self, input_file = None, parse_func = None, checklist = None, sep = "\t"):
        if input_file is None:
            input_file = self.input_zfile
            assert input_file is not None, "You did not provide a .zipfile file"
        z = zipfile.ZipFile(os.path.join(self.input_zfile))
        z_fs = [x for x in z.namelist() if x.endswith("tsv") or x.endswith("csv")]
        if checklist is not None:
            z_fs = [x for x in z_fs if x in checklist]
            print(z_fs)
            print(f"Length of {len(z_fs)} after using checklist")
        print(f"Parsing files to vfamcdr3 format; Accelerating with parmap and {self.cpus} cpus")
        if parse_func is not None:
            vfamcdr3_v2_single_ = partial(vfamcdr3_v2_single, parse_func= parse_func, sep = sep)
            parmap.map(vfamcdr3_v2_single_, z_fs, 
                zfile =self.input_zfile, 
                outdir =self.outdir_vfamcdr3, 
                pm_pbar = True, pm_processes = self.cpus)

    def subsample_vfamcdr3_v2_parmap(self, filelist, 
            n_samples = [10_000, 20_000, 30_000, 50_000, 100_000, 200_000, 300_000, 400_000, 500_000], 
            seed_i =1):
        results = parmap.map(
            subsample_vfamcdr3, #(fp, outdir_vfamcdr3, outdir_vfamcdr3_subsample, n_samples, seed_i=0),
            filelist,
            self.outdir_vfamcdr3,
            self.outdir_vfamcdr3_subsample,
            n_samples,
            seed_i,
            pm_pbar=True,  # Show progress bar
            pm_processes=self.cpus  # Use all but one core
        )
        print(f"Subsampled Files: {self.outdir_vfamcdr3_subsample}")


    def combine_stratv(self, pattern = '_combined.csv', checklist = None):
        print(f"Combining into single Vfam, files from\n\t -- {self.input_zfile}")
        print(f"Using Checklist")
        if self.cpus == 1:
            with Bar('Processing', max=len(self.vlist)) as bar:
                for v in self.vlist:
                    print(f" -- {v}")
                    fs = os.listdir(os.path.join(self.outdir_stratv, v) )
                    if checklist is not None:
                        fs = [x for x in fs if x.replace(f"{v}_","") in checklist]
                        print(f"Length of {len(fs)} after using checklist")
                        print(fs)
                    s = list()
                    for f in fs:
                        s.append(pd.read_csv(os.path.join(self.outdir_stratv,v,f)))
                    dfv = pd.concat(s).reset_index(drop = True)
                    ix1 = dfv['amino_acid'].str.len() >= 6
                    ix2 = dfv['amino_acid'].notna()
                    dfv = dfv[ix1&ix2].reset_index(drop = True)
                    dfv.to_csv(os.path.join(self.outdir_stratv_combined, f"{v}{pattern}"), 
                        index = False)
                    bar.next()
                    
        elif self.cpus > 1:
            print(f"Combining files; Accelerating with parmap and {self.cpus} cpus")
            folders = [os.path.join(self.outdir_stratv, v) for v in self.vlist]
            dests =   [os.path.join(self.outdir_stratv_combined, f"{v}{pattern}") for v in self.vlist]
            
            if checklist is not None:
                x = [(f,d, checklist) for f,d  in zip(folders, dests)]
                parmap.map(combine_to_dest2, x, pm_pbar = True, pm_processes = self.cpus)
            else:
                x = [(f,d) for f,d  in zip(folders, dests)]
                parmap.map(combine_to_dest, x, pm_pbar = True, pm_processes = self.cpus)

    def build_feature_occurance_matrix(self, 
                                       query_df, 
                                       filelist=None, 
                                       get_col = "productive_frequency",
                                       min_value = None,
                                       on = 'vfamcdr3', 
                                       cpus = None, 
                                       sep = ",",
                                       add_counts = False):
        """
        For use with HLApredict (must provide query_df from a specific model)
        """
        if cpus is None:
            cpus = self.cpus
        from tcrtest.find_across_cohort import unique_clone_counts_from_folder_files
        from tcrtest.tabulate import tabify, tabify1
        if filelist is None:
            filelist = [os.path.join(self.outdir_vfamcdr3, x) for x in os.listdir(self.outdir_vfamcdr3)]

        Q = query_df
        Q1 = Q[Q['search'] == "edit1"]
        Q0 = Q[Q['search'] == "edit0"]
                # For exact features
        #import pdb; pdb.set_trace()
        X0 = tabify(query = Q0, 
            filelist = filelist, 
            on = on, 
            get_col = get_col, 
            min_value = min_value,
            sep =  sep , 
            cpus = cpus)
        # For fuzzy feature
        X1 = tabify1(query = Q1, 
            filelist = filelist, 
            on = on, 
            get_col = get_col, 
            min_value = min_value,
            sep = sep , 
            cpus = cpus)
        #import pdb; pdb.set_trace()
        X01 = pd.concat([X1,X0], axis = 0, ignore_index=True)
        
        if min_value is None:
            dfu = unique_clone_counts_from_folder_files(self.outdir_vfamcdr3, 
            endswith ='.csv', 
            sep = sep,
            cpus = cpus)
            dfu['log10unique2'] = dfu['log10unique'] **2
        else:
            from tqdm import tqdm
            # This will deal with min_value

            # Define output directory
            outdir = '/fh/fast/gilbert_p/kmayerbl/cross_study_reference/vfamcdr3' # Ensure 'v' is properly defined
            # Use parmap to parallelize processing with 24 CPUs
            #import pdb; pdb.set_trace()
            your_results = parmap.map(get_dfu_info_above_min_val, filelist, get_col = get_col, min_value = min_value, pm_pbar=True, pm_processes=cpus)
            dfu = pd.DataFrame(your_results, columns = ['sample_id','m1','m2','u1','u2'])
            dfu['sample_id'] = dfu['sample_id'].str.replace(".csv","")
            dfu['log10unique'] = np.log10(dfu['u2']) # log10 unique of retained amount not total in input.
            dfu['log10unique2'] = dfu['log10unique'] **2
            dfu = dfu[['sample_id','log10unique','log10unique2']]
            dfu.index = dfu['sample_id']
            #print(dfu)

        if add_counts:
            C = dfu[['log10unique']].transpose().reset_index(drop = True)
            extra_df = pd.DataFrame({'binary':sorted(query_df.binary.unique().tolist())})
            n_row_extra = extra_df.shape[0]
            C_rep = pd.concat([C] * n_row_extra, ignore_index=True)
            C_rep = C_rep[X01.columns]
            assert (C_rep.columns == X01.columns).all()
            X = pd.concat([X01, C_rep]).reset_index(drop = True)
        else:
            X = X01
        #Not always true. assert X.shape[0] == query_df.shape[0]
        return X, dfu
    # def build_feature_occurance_matrix(self, 
    #                                    query_df, 
    #                                    filelist=None, 
    #                                    get_col = "productive_frequency",
    #                                    min_value = None,
    #                                    on = 'vfamcdr3', 
    #                                    cpus = None, 
    #                                    add_counts = False):
    #     """
    #     For use with HLApredict (must provide query_df from a specific model)
    #     """
    #     if cpus is None:
    #         cpus = self.cpus
    #     from tcrtest.find_across_cohort import unique_clone_counts_from_folder_files
    #     from tcrtest.tabulate import tabify, tabify1
    #     if filelist is None:
    #         filelist = [os.path.join(self.outdir_vfamcdr3, x) for x in os.listdir(self.outdir_vfamcdr3)]

    #     # For exact features
    #     X0 = tabify(query = query_df, 
    #         filelist = filelist, 
    #         on = on, 
    #         get_col = get_col, 
    #         min_value = min_value,
    #         sep = ",", 
    #         cpus = cpus)
    #     # For fuzzy feature
    #     X1 = tabify1(query = query_df, 
    #         filelist = filelist, 
    #         on = on, 
    #         get_col = get_col, 
    #         min_value = min_value,
    #         sep = ",", 
    #         cpus = cpus)


    #     if min_value is None:
    #         dfu = unique_clone_counts_from_folder_files(self.outdir_vfamcdr3, 
    #         endswith ='.csv', 
    #         cpus = cpus)
    #         dfu['log10unique2'] = dfu['log10unique'] **2
    #     else:
    #         from tqdm import tqdm
    #         # This will deal with min_value

    #         # Define output directory
    #         outdir = '/fh/fast/gilbert_p/kmayerbl/cross_study_reference/vfamcdr3' # Ensure 'v' is properly defined
    #         # Use parmap to parallelize processing with 24 CPUs
    #         your_results = parmap.map(get_dfu_info_above_min_val, filelist, get_col, min_value, pm_pbar=True, pm_processes=cpus)
    #         dfu = pd.DataFrame(your_results, columns = ['sample_id','m1','m2','u1','u2'])
    #         dfu['sample_id'] = dfu['sample_id'].str.replace(".csv","")
    #         dfu['log10unique'] = np.log10(dfu['u2']) # log10 unique of retained amount not total in input.
    #         dfu['log10unique2'] = dfu['log10unique'] **2
    #         dfu = dfu[['sample_id','log10unique','log10unique2']]
    #         dfu.index = dfu['sample_id']
    #         #print(dfu)

    #     ix1 = query_df[query_df.search == "edit1"].index.to_list()
    #     ix0 = query_df[query_df.search == "edit0"].index.to_list()
    #     print(ix1)
    #     print(ix0)
    #     # COMBINE THE EDIT1 and EDIT0 MATRICES
    #     X01 = pd.concat([X1.iloc[ix1,:],X0.iloc[ix0,:]])
    #     C = dfu[['log10unique']].transpose().reset_index(drop = True)
    #     extra_df = pd.DataFrame({'binary':sorted(query_df.binary.unique().tolist())})
    #     n_row_extra = extra_df.shape[0]
    #     C_rep = pd.concat([C] * n_row_extra, ignore_index=True)
    #     C_rep = C_rep[X01.columns]
    #     assert (C_rep.columns == X01.columns).all()
    #     if add_counts:
    #         X = pd.concat([X01, C_rep]).reset_index(drop = True)
    #     else:
    #         X = X01
    #     #Not always true. assert X.shape[0] == query_df.shape[0]
    #     return X, dfu

    def save_occurrance_matrix(self, df, outfile, outdir = None):
        # temporary one if mispelled occurance vs. occurence
        self.save_occurence_matrix(df=df, outfile=outfile, outdir = outdir)

    def save_occurrence_matrix(self, df, outfile, outdir = None):
        """
        Saves a given DataFrame as a sparse Compressed Sparse Row (CSR) matrix 
        and exports its column names as a separate CSV file.

        Parameters:
        -----------
        df : pandas.DataFrame
            The input DataFrame to be saved in sparse format.
        outfile : str
            The filename for the saved matrix.
        outdir : str, optional
            The directory where the files will be saved. If not provided, 
            defaults to `self.project_folder`.

        Returns:
        --------
        None
            The function saves two files:
            1. `outfile` (a .npz file containing the sparse matrix)
            2. `outfile.columns.csv` (a CSV file containing column names)
        """
        if outdir is None:
            outdir= self.project_folder
        from scipy.sparse import csr_matrix, save_npz
        Xcsr = csr_matrix(df.values)
        out = os.path.join(outdir, outfile)
        print(f"Writing [df] as sparse csr_mat to {out}")
        save_npz(file=os.path.join(outdir, outfile), matrix =Xcsr)
        print(f"Writing [df].columns as .sv {out}.columns.csv")
        pd.Series(df.columns).to_csv(os.path.join(outdir, f"{outfile}.columns.csv"))

    def load_occurrance_matrix(self, infile, indir = None):
        # temporary one if mispelled occurance vs. occurence
        self.load_occurrence_matrix(self, infile =infile, indir=indir)

    def load_occurrence_matrix(self, infile, indir=None):
        """
        Loads a sparse matrix stored in .npz format and reconstructs it as a pandas DataFrame.

        Parameters:
        -----------
        infile : str
            Filename (without extension) of the saved sparse matrix.
        indir : str, optional
            Directory where the file is located. Defaults to `self.project_folder` if not provided.

        Returns:
        --------
        pandas.DataFrame
            A reconstructed DataFrame with the original column names.

        Example
        -------
        K = pd.DataFrame({'a':[1,0,1],'b':[2,0,0]})
        v.save_occurance_matrix(df = K, outfile = "K.npz", outdir = None)
        v.load_occurrence_matrix(infile= "K.npz")
        """
        from scipy.sparse import load_npz
        if indir is None:
            indir = self.project_folder
        npz_path = os.path.join(indir, f"{infile}")
        columns_path = os.path.join(indir, f"{infile}.columns.csv")
        Xcsr = load_npz(npz_path)
        columns = pd.read_csv(columns_path).iloc[:, 1].tolist()
        #columns = pd.read_csv(columns_path, index_col=0, header=None, squeeze=True).tolist()
        # Reconstruct the DataFrame
        df = pd.DataFrame.sparse.from_spmatrix(Xcsr)
        df.columns = columns
        return df

    def assemble_association_files(self, endswith = "1e6.csv"):
        fs1 = [x for x in os.listdir(self.outdir_stratv_hla) if x.endswith(endswith ) and x.find("edit1") !=-1]
        fs0 = [x for x in os.listdir(self.outdir_stratv_hla) if x.endswith(endswith ) and x.find("edit0") !=-1]
        df1 = pd.concat([pd.read_csv(os.path.join(self.outdir_stratv_hla, x)) for x in fs1]).sort_values('p_exact').reset_index(drop = True)
        df0 = pd.concat([pd.read_csv(os.path.join(self.outdir_stratv_hla, x)) for x in fs0]).sort_values('p_exact').reset_index(drop = True)

        df1['prev_hla'] = df1.a / (df1.a + df1.c)
        df1['prev_out'] = df1.b / (df1.b + df1.d)
        #df1['key'] = df1['v']+"_"+df1['j']+"_"+df1['cdr3']+"__" + df1['binary'] 

        df0['prev_hla'] = df0.a / (df0.a + df0.c)
        df0['prev_out'] = df0.b / (df0.b + df0.d)
        #df0['key'] = df0['v']+"_"+df0['j']+"_"+df0['cdr3']+"__" + df0['binary'] 

        if 'V' in df0.columns:
            df0['vfamcdr3'] = df0['V']+df0['cdr3']
        else:
            df0['V'] = df0.v.str.split("-", expand = True)[0].str.replace('TCRB','')
            df0['vfamcdr3'] = df0['V']+df0['cdr3']
        
        if 'V' in df1.columns:
            df1['vfamcdr3'] = df1['V']+df1['cdr3']
        else:
            df1['V'] = df1.v.str.split("-", expand = True)[0].str.replace('TCRB','')
            df1['vfamcdr3'] = df1['V']+df1['cdr3']

        df0hq = df0.query('p_exact < 1e-8').query('prev_hla > 0.05').query('prev_out < .1').reset_index(drop = True).sort_values('p_exact').groupby('vfamcdr3').head(1)
        df1hq = df1.query('p_exact < 1e-8').query('prev_hla > 0.05').query('prev_out < .1').reset_index(drop = True).sort_values('p_exact').groupby('vfamcdr3').head(1)

        self.df0, self.df1, self.df0hq, self.df1hq = df0, df1, df0hq, df1hq

        return df0, df1, df0hq, df1hq


    def assemble_association_files_tag(self, endswith = "1e6.csv", tag_col = "V"):

        fs1 = [x for x in os.listdir(self.outdir_stratv_hla) if x.endswith(endswith ) and x.find("edit1") !=-1]
        fs0 = [x for x in os.listdir(self.outdir_stratv_hla) if x.endswith(endswith ) and x.find("edit0") !=-1]

        df1 = pd.concat([read_and_tag(os.path.join(self.outdir_stratv_hla, x), tag_col = tag_col ) for x in fs1]).sort_values('p_exact').reset_index(drop = True)
        df0 = pd.concat([read_and_tag(os.path.join(self.outdir_stratv_hla, x), tag_col = tag_col ) for x in fs0]).sort_values('p_exact').reset_index(drop = True)

        df1['prev_hla'] = df1.a / (df1.a + df1.c)
        df1['prev_out'] = df1.b / (df1.b + df1.d)
        df1['key'] = df1['v']+"_"+df1['j']+"_"+df1['cdr3']+"__" + df1['binary'] 

        df0['prev_hla'] = df0.a / (df0.a + df0.c)
        df0['prev_out'] = df0.b / (df0.b + df0.d)
        df0['key'] = df0['v']+"_"+df0['j']+"_"+df0['cdr3']+"__" + df0['binary'] 


        #df0['V'] = df0.v.str.split("-", expand = True)[0].str.replace('TCRB','')
        df0['vfamcdr3'] = df0['V']+df0['cdr3']

        #df1['V'] = df1.v.str.split("-", expand = True)[0].str.replace('TCRB','')
        df1['vfamcdr3'] = df1['V']+df1['cdr3']

        df0hq = df0.query('p_exact < 1e-8').query('prev_hla > 0.05').query('prev_out < .1').reset_index(drop = True).sort_values('p_exact').groupby('vfamcdr3').head(1)
        df1hq = df1.query('p_exact < 1e-8').query('prev_hla > 0.05').query('prev_out < .1').reset_index(drop = True).sort_values('p_exact').groupby('vfamcdr3').head(1)

        self.df0, self.df1, self.df0hq, self.df1hq = df0, df1, df0hq, df1hq

        return df0, df1, df0hq, df1hq

    def with_single_machine(self, hummingbird = 'run_direct_finder'):
        with Bar('Processing', max=len(self.launch_paths[hummingbird])) as bar:
            cnt = 1
            for bash_script_path in self.launch_paths[hummingbird]:
                try:
                    cmd = f"bash {bash_script_path}"
                    print("\n")
                    print(cmd)
                    result = subprocess.run(cmd, check=True, capture_output=False, text=True, shell = True)
                except subprocess.CalledProcessError as e:
                    print(f"Error running {script}:\n{e.stderr}")
                bar.next()

    def with_slurm(self, hummingbird = 'run_direct_finder'):
        with Bar('Processing', max=len(self.launch_paths[hummingbird])) as bar:
            for bash_script_path in self.launch_paths[hummingbird]:
                print(f"Executing {bash_script_path}")
                subprocess.run(["chmod", "+x", bash_script_path])
                subprocess.run(["sbatch",      bash_script_path])
                bar.next()

    def launch_with_single_machine(self):
        # IF YOU HAVE ONLY ONE MACHINE MACHINE, LAUNCH JOB ONE AT A TIME AND WAIT
        with Bar('Processing', max=len(self.launch_paths['run_nx'])) as bar:
            cnt = 1
            for a,b,c in zip(self.launch_paths['run_nx'], self.launch_paths['binary_edit0'],self.launch_paths['binary_edit1']):
                cnt = cnt + 1
                for script in [a,b,c]:
                    subprocess.run(['chmod', '+x', script], check=True, capture_output=True, text=True)
                try:
                    cmd = f"bash {a} && {b} && {c}"
                    print("\n")
                    print(cmd)
                    result = subprocess.run(cmd, check=True, capture_output=False, text=True, shell = True)
                except subprocess.CalledProcessError as e:
                    print(f"Error running {script}:\n{e.stderr}")
                bar.next()

    def launch_with_single_machine_gliph(self):
        # IF YOU HAVE ONLY ONE MACHINE MACHINE, LAUNCH JOB ONE AT A TIME AND WAIT
        with Bar('Processing', max=len(self.launch_paths['run_gliph'])) as bar:
            cnt = 1
            assert len(self.launch_paths['run_gliph']) == len(self.launch_paths['binary_gliph_nr'])
            for a,b,c in zip(self.launch_paths['run_gliph'], self.launch_paths['binary_gliph_nr'],self.launch_paths['binary_gliph_all']):
                cnt = cnt + 1
                for script in [a,b,c]:
                    subprocess.run(['chmod', '+x', script], check=True, capture_output=True, text=True)
                try:
                    cmd = f"bash {a} && {b} && {c}"
                    print("\n")
                    print(cmd)
                    result = subprocess.run(cmd, check=True, capture_output=False, text=True, shell = True)
                except subprocess.CalledProcessError as e:
                    print(f"Error running {script}:\n{e.stderr}")
                bar.next()

    def launch_with_slurm_gliph(self, launch = True):
        # IF YOU HAVE A CLUSTER YOU CAN LAUNCH THESE ALL AT ONCE AS MINI WORKFLOWS
        workflows_by_strata = list()
        with Bar('Processing', max=len(self.launch_paths['run_gliph'])) as bar:
            cnt = 1
            assert len(self.launch_paths['run_gliph']) == len(self.launch_paths['binary_gliph_nr'])
            for a,b,c in zip(self.launch_paths['run_gliph'], self.launch_paths['binary_gliph_nr'],self.launch_paths['binary_gliph_all']):
                cnt = cnt + 1
                for script in [a,b,c]:
                    subprocess.run(['chmod', '+x', script], check=True, capture_output=True, text=True)

                workflow_script = f"""first_job_id=$(sbatch --parsable {a})
echo "First job submitted with ID: $first_job_id"
echo {a}

second_job_id=$(sbatch --parsable --dependency=afterok:$first_job_id {b})
echo "Second job submitted with ID: $second_job_id"
echo {b}

third_job_id=$(sbatch --parsable --dependency=afterok:$first_job_id {c})
echo "Third job submitted with ID: $third_job_id"
echo {c}

echo "NOTE: The Second and Third job will start ONLY after the first job completes successfully."
"""
                strata = os.path.basename(a).split("_")[0]
                fp = os.path.join(self.bash_path, f"{strata}.gliph.workflow.sh")
                workflows_by_strata.append(fp)
                with open(fp, "w") as wf:
                    wf.write(workflow_script)
                print(f"\t -- Wrote {fp}")
                bar.next()
        if launch:
            with Bar('Launching workflows', max=len(workflows_by_strata)) as bar:
                for script in workflows_by_strata :
                    try:
                        result = subprocess.run(['bash', script], check=True, capture_output=True, text=True)
                        print(f"\nOutput of {script}:\n{result.stdout}")
                    except subprocess.CalledProcessError as e:
                        print(f"\nError running {script}:\n{e.stderr}")
                    bar.next()

    def launch_with_slurm(self):
        # IF YOU HAVE A CLUSTER YOU CAN LAUNCH THESE ALL AT ONCE AS MINI WORKFLOWS
        workflows_by_strata = list()
        with Bar('Processing', max=len(self.launch_paths['run_nx'])) as bar:
            cnt = 1
            for a,b,c in zip(self.launch_paths['run_nx'], self.launch_paths['binary_edit0'],self.launch_paths['binary_edit1']):
                cnt = cnt + 1
                for script in [a,b,c]:
                    subprocess.run(['chmod', '+x', script], check=True, capture_output=True, text=True)

                workflow_script = f"""first_job_id=$(sbatch --parsable {a})
echo "First job submitted with ID: $first_job_id"
echo {a}

second_job_id=$(sbatch --parsable --dependency=afterok:$first_job_id {b})
echo "Second job submitted with ID: $second_job_id"
echo {b}

third_job_id=$(sbatch --parsable --dependency=afterok:$first_job_id {c})
echo "Third job submitted with ID: $third_job_id"
echo {c}

echo "NOTE: The Second and Third job will start ONLY after the first job completes successfully."
"""
                strata = os.path.basename(a).split("_")[0]
                fp = os.path.join(self.bash_path, f"{strata}.workflow.sh")
                workflows_by_strata.append(fp)
                with open(fp, "w") as wf:
                    wf.write(workflow_script)
                print(f"\t -- Wrote {fp}")
                bar.next()

        with Bar('Launching workflows', max=len(workflows_by_strata)) as bar:
            for script in workflows_by_strata :
                try:
                    result = subprocess.run(['bash', script], check=True, capture_output=True, text=True)
                    print(f"\nOutput of {script}:\n{result.stdout}")
                except subprocess.CalledProcessError as e:
                    print(f"\nError running {script}:\n{e.stderr}")
                bar.next()


    def run_direct_finder(self, 

        subject_binary_file,
        binary_variables,
        pattern = "_combined.csv",
        query_cdr3_col='amino_acid',
        query_v_col='v_gene',
        query_j_col='j_gene',
        query_vfam_col='v',
        sample_id_col='sample_id',
        min_occur=1,
        min_collisions=5,
        min_pub=5,
        max_pval = 1E-3,
        max_pval_override0 = 1E-3,
        max_pval_override1 = 1E-3,
        allow_missing=False,
        partition = "short",
        force = False, 
        launch = False, 
        setup_commands = None): # e.g., source ~/.bashrc && conda activate tcrdist311
    
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            if tasks < 2:
                tasks = 2
                # take advantage of parmap
            if not os.path.isfile(opj(self.outdir_stratv_npz, f"{v}.edit1.binvar.csv")) or force:
                bp = run_direct_finder(
                    bash_path =  opj(self.project_folder, "bash"),
                    infile=opj(self.outdir_stratv_combined, f"{v}{pattern}"),
                    binary_matrix=subject_binary_file,
                    binary_variables=binary_variables,
                    outfile_binary_edit1=opj(self.project_folder, "stratv_hla", f"{v}.edit1.binvar.csv"),
                    outfile_binary_edit0=opj(self.project_folder, "stratv_hla", f"{v}.edit0.binvar.csv"),
                    sep=',',
                    seqs_col='amino_acid',
                    cpus=tasks,
                    query_cdr3_col=query_cdr3_col,
                    query_v_col=query_v_col,
                    query_j_col=query_j_col,
                    query_vfam_col=query_vfam_col,
                    sample_id_col=sample_id_col,
                    min_occur=min_occur,
                    min_collisions=min_collisions,
                    min_pub=min_pub,
                    max_pval=max_pval,
                    max_pval_override0 = max_pval_override0,
                    max_pval_override1 = max_pval_override1,
                    allow_missing = allow_missing,
                    tasks = tasks,
                    mem = mem,
                    time = "02:00:00",
                    partition = partition,
                    launch = launch)
                bash_paths.append(bp)

        self.launch_paths['run_direct_finder'] = bash_paths


    def run_nx(self, force = False, pattern = "_combined.CLEAN.csv", launch = False, use_dask= False):
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
                
            print(v, f,mem,tasks,num,nn_min)
            print(f"USING DASK: {use_dask}, TASKS: {tasks}")
            if int(tasks) < 2:
                print(f"USING DASK: {use_dask}, TASKS: {tasks}")
                use_dask_temp = False
            else: 
                use_dask_temp = use_dask

            if not os.path.isfile(opj(self.outdir_stratv_npz, f"{v}.edit1.npz")) or force:
                bp = run_nx(bash_path = opj(self.project_folder, "bash"),
                          infile = opj(self.outdir_stratv_combined, f"{v}{pattern}"),
                          outfile_csrmat_edit0 = opj(self.project_folder, "stratv_npz", f"{v}.edit0.npz"),
                          outfile_csrmat_edit1 = opj(self.project_folder, "stratv_npz", f"{v}.edit1.npz"),
                          tasks = tasks,
                          mem = mem,
                          time = '02:00:00',
                          partition = "short",
                          seqs_col = "amino_acid",
                          sep = ",",
                          min_occur = 1,
                          use_dask = use_dask_temp,
                          dask_report_name = opj(self.project_folder, "bash", f"{v}_dask_report.html"),
                          launch = launch)
                bash_paths.append(bp)

        self.launch_paths['run_nx'] = bash_paths


    def run_gliph(self, force = False, pattern = "_combined.CLEAN.csv", launch = False, 
        fdr_max = 0.001, left_trim = 3, right_trim = 0, ks = '7,8,9,10',
        infile_reference =  "/fh/fast/gilbert_p/kmayerbl/tcrtest_data/20240715_olga_random_10M_amino_acid_V_background.csv"):
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            if not os.path.isfile(opj(self.outdir_stratv_npz, f"{v}.gliph_all.npz")) or force:
                bp = run_gliph(bash_path = opj(self.project_folder, "bash"),
                                infile = opj(self.outdir_stratv_combined, f"{v}{pattern}"),
                                stratv = v,
                                outfile_csv_gliph       = opj(self.outdir_stratv_combined, f"{v}{pattern}.gliph_all.csv"),
                                outfile_csrmat_gliph    = opj(self.project_folder, "stratv_npz", f"{v}.gliph_all.npz"),
                                outfile_csv_gliph_nr    = opj(self.outdir_stratv_combined, f"{v}{pattern}.gliph_nr.csv"),
                                outfile_csrmat_gliph_nr = opj(self.project_folder, "stratv_npz", f"{v}.gliph_nr.npz"),
                                infile_reference = infile_reference,
                                tasks = tasks,
                                mem = mem,
                                ks = ks,
                                left_trim = left_trim ,
                                right_trim = right_trim ,
                                fdr_max = fdr_max,
                                min_count = 5,
                                launch = launch)
                bash_paths.append(bp)
        self.launch_paths['run_gliph'] = bash_paths

    def run_binary_association_gliph_all(self, subject_binary_file, binary_variables, 
        sample_id_col = 'sample_id', pattern = "_combined.CLEAN.csv",force = False, launch = False):
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            bp = run_binary_test(bash_path =opj(self.project_folder, "bash"),
                    binary_matrix = subject_binary_file,
                    binary_variables = binary_variables,
                    csr_mat = opj(self.outdir_stratv_npz, f"{v}.gliph_all.npz"),
                    query_file = opj(self.outdir_stratv_combined, f"{v}{pattern}.gliph_all.csv"),
                    subject_file = opj(self.outdir_stratv_combined, f"{v}{pattern}"), 
                    outfile = opj(self.project_folder, "stratv_hla", f"{v}.gliph_all.csv"),
                    tasks = 2,
                    mem = 32,
                    time = "02:00:00",
                    partition = "short",
                    query_cdr3_col = 'amino_acid',
                    query_v_col = 'v_gene',
                    query_j_col = 'j_gene',
                    sample_id_col = sample_id_col,
                    min_nn = 5, 
                    launch = launch)
            bash_paths.append(bp)
        self.launch_paths['binary_gliph_all'] = bash_paths


    def run_binary_association_gliph_nr(self, subject_binary_file, binary_variables, 
        sample_id_col = 'sample_id', pattern = "_combined.CLEAN.csv",force = False, launch = False):
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            bp = run_binary_test(bash_path =opj(self.project_folder, "bash"),
                    binary_matrix = subject_binary_file,
                    binary_variables = binary_variables,
                    csr_mat = opj(self.outdir_stratv_npz, f"{v}.gliph_nr.npz"),
                    query_file = opj(self.outdir_stratv_combined, f"{v}{pattern}.gliph_nr.csv"),
                    subject_file = opj(self.outdir_stratv_combined, f"{v}{pattern}"), 
                    outfile = opj(self.project_folder, "stratv_hla", f"{v}.gliph_nr.csv"),
                    tasks = 1,
                    mem = 16,
                    time = "02:00:00",
                    partition = "short",
                    query_cdr3_col = 'amino_acid',
                    query_v_col = 'v_gene',
                    query_j_col = 'j_gene',
                    sample_id_col = sample_id_col,
                    min_nn = 5, 
                    launch = launch)
            bash_paths.append(bp)
        self.launch_paths['binary_gliph_nr'] = bash_paths

    def run_nx_and_binary(self,  subject_binary_file, binary_variables, sample_id_col = 'sample_id',pattern = "_combined.CLEAN.csv",force = False, launch = False, use_dask= False):
        # DON"T USE
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            if not os.path.isfile(opj(self.outdir_stratv_npz, f"{v}.edit1.npz")) or force:
                bp = run_nx_and_binary_test(bash_path = opj(self.project_folder, "bash"),
                          infile = opj(self.outdir_stratv_combined, f"{v}{pattern}"),
                          outfile_csrmat_edit0 = opj(self.project_folder, "stratv_npz", f"{v}.edit0.npz"),
                          outfile_csrmat_edit1 = opj(self.project_folder, "stratv_npz", f"{v}.edit1.npz"),
                          tasks = tasks,
                          mem = mem,
                          time = '02:00:00',
                          partition = "short",
                          seqs_col = "amino_acid",
                          sep = ",",
                          min_occur = 1,
                          use_dask = use_dask,
                          dask_report_name = opj(self.project_folder, "bash", f"{v}_dask_report.html"),
                            binary_matrix = subject_binary_file,
                            binary_variables = binary_variables,
                            query_file = opj(self.outdir_stratv_combined, f"{v}{pattern}") ,
                            subject_file = opj(self.outdir_stratv_combined, f"{v}{pattern}"), 
                            outfile_edit0 = opj(self.project_folder, "stratv_hla", f"{v}.edit0.csv"),
                            outfile_edit1 = opj(self.project_folder, "stratv_hla", f"{v}.edit1.csv"),
                            query_cdr3_col = 'amino_acid',
                            query_v_col = 'v_gene',
                            query_j_col = 'j_gene',
                            sample_id_col = sample_id_col,
                            min_nn = 5, 
                            launch = launch)
                bash_paths.append(bp)
        self.launch_paths['run_nx'] = bash_paths



    def run_binary_association_edit1(self, subject_binary_file, binary_variables, 
        sample_id_col = 'sample_id', pattern = "_combined.CLEAN.csv",force = False, launch = False):
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            bp = run_binary_test(bash_path =opj(self.project_folder, "bash"),
                    binary_matrix = subject_binary_file,
                    binary_variables = binary_variables,
                    csr_mat = opj(self.outdir_stratv_npz, f"{v}.edit1.npz"),
                    query_file = opj(self.outdir_stratv_combined, f"{v}{pattern}") ,
                    subject_file = opj(self.outdir_stratv_combined, f"{v}{pattern}"), 
                    outfile = opj(self.project_folder, "stratv_hla", f"{v}.edit1.csv"),
                    tasks = tasks,
                    mem = mem,
                    time = "02:00:00",
                    partition = "short",
                    query_cdr3_col = 'amino_acid',
                    query_v_col = 'v_gene',
                    query_j_col = 'j_gene',
                    sample_id_col = sample_id_col,
                    min_nn = 5, 
                    launch = launch)
            bash_paths.append(bp)
        self.launch_paths['binary_edit1'] = bash_paths
    
    def run_binary_association_edit0(self, 
        subject_binary_file, binary_variables, 
        sample_id_col = 'sample_id', 
        pattern = "_combined.CLEAN.csv",force = False, launch = False):
        bash_paths = list()
        for v,f,mem,tasks,num,nn_min in self.vfam_filelist:
            print(v, f,mem,tasks,num,nn_min)
            bp = run_binary_test(bash_path =opj(self.project_folder, "bash"),
                    binary_matrix = subject_binary_file,
                    binary_variables = binary_variables,
                    csr_mat = opj(self.outdir_stratv_npz, f"{v}.edit0.npz"),
                    query_file = opj(self.outdir_stratv_combined, f"{v}{pattern}") ,
                    subject_file = opj(self.outdir_stratv_combined, f"{v}{pattern}"), 
                    outfile = opj(self.project_folder, "stratv_hla", f"{v}.edit0.csv"),
                    tasks = tasks,
                    mem = mem,
                    time = "02:00:00",
                    partition = "short",
                    query_cdr3_col = 'amino_acid',
                    query_v_col = 'v_gene',
                    query_j_col = 'j_gene',
                    sample_id_col = sample_id_col,
                    min_nn = 5, 
                    launch = launch)
            bash_paths.append(bp)
        self.launch_paths['binary_edit0'] = bash_paths


    def count_lines_with_wc(self, file_path):
        result = subprocess.run(['wc', '-l', file_path], stdout=subprocess.PIPE, text=True)
        line_count = result.stdout.split()[0]
        print(f"wc -l ->>> {file_path} --> {line_count} lines")
        return int(line_count)

    def list_combined_vfam_files(self):
        [print(f" -- {f}") for f in os.listdir(self.outdir_stratv_combined)]
        return os.listdir(self.outdir_stratv_combined)

    def get_combined_vfam_filelist(self, pattern="_combined.csv",fp = None):
        if self.cpus == 1:
            if fp is None:
                fp = self.outdir_stratv_combined
            fs = [f for f in os.listdir(fp) if f.endswith(pattern)]
            #print(fs)
            store = list()
            with Bar('Getting clones per file', max=len(fs)) as bar:
                for f in fs:
                    res = re.search(string = f , pattern = "(^V[0-9]{2}).*")
                    if res is not None:
                        v = res.groups()[0]
                    else:
                        continue
                    line_count = self.count_lines_with_wc(opj(fp, f))
                    mem,tasks,nn_min = line_count_to_mem_tasks(line_count)
                    print((v, f,mem,tasks,line_count, nn_min))
                    bar.next()
                    store.append((v, f,mem,tasks,line_count, nn_min))
            self.vfam_filelist = store
            return store
        else:
            if fp is None:
                fp = self.outdir_stratv_combined
            print(f"Enumerating clone per file; Accelerating with parmap and {self.cpus} cpus")
            fs = [opj(fp, f) for f in os.listdir(self.outdir_stratv_combined) if f.endswith(pattern)]
            #print(fs)
            line_counts = parmap.map(self.count_lines_with_wc, 
                fs, 
                pm_pbar= True, 
                pm_processes=self.cpus)
            store = list()
            for file_p,lc in zip(fs,line_counts):
                f = os.path.basename(file_p)
                res = re.search(string = f , pattern = "(^V[0-9]{2}).*")
                if res is not None:
                    v = res.groups()[0]
                    mem,tasks,nn_min= line_count_to_mem_tasks(lc)
                    print((v, f, mem,tasks,lc,nn_min))
                    store.append((v, f, mem,tasks,lc, nn_min))
            self.vfam_filelist = store
            return store

def line_count_to_mem_tasks(line_count):
    if line_count < 250000:
        mem = 16
        tasks = 1
        nn_min = 1
    elif line_count < 500000:
        mem = 32
        tasks = 2
        nn_min = 1
    elif line_count < 1000000:
        mem = 48
        tasks = 4
        nn_min = 1
    elif line_count < 2000000:
        mem = 96
        tasks = 6
        nn_min = 5
    elif line_count < 4000000:
        mem = 160
        tasks = 8
        nn_min = 5
    elif line_count < 5000000:
        mem = 200
        tasks = 10
        nn_min = 5
    elif line_count < 6000000:
        mem = 320
        tasks = 16
        nn_min = 10
    elif line_count < 8000000:
        mem = 400
        tasks = 20
        nn_min = 10
    elif line_count < 9000000:
        mem   = 480
        tasks = 24
        nn_min = 20
    elif line_count < 10000000:
        mem   = 600
        tasks = 30
        nn_min = 20
    elif line_count > 10000000:
        mem   = 600
        tasks = 34
        nn_min = 20
    return((mem,tasks,nn_min))

def get_revelant_binaries(subject_binary_file, min_allele_prev = .1):
    b = pd.read_csv(subject_binary_file, sep = "\t", index_col = 0)
    ap = b.sum()/b.shape[0]
    return ",".join(sorted(ap[ap > min_allele_prev].\
        sort_values(ascending = False).\
        index.to_list()))


def mkdir_if_needed(x):
    if not os.path.isdir(x):
        print(f"CREATING DIRECTORY: {x}")
        os.mkdir(x)
    return True

def check_exists(x):
    if os.path.isfile(x) or os.path.isdir(x):
        #print(f"CONFIRMED EXISTENCE OF: {x} ")
        return True
    else:
        return False

def assert_exists(x):
    #print(f"CONFIRMED EXISTENCE OF: {x} ")
    assert os.path.isfile(x) or os.path.isdir(x)
    return True


def if_not_create_dir(x):
    if not os.path.isdir(x): 
        os.mkdir(x)
    assert_exists(x)
    return True

def view_raw_file(f,zfile):
    z = zipfile.ZipFile(os.path.join(zfile))
    file = z.open(f)
    df = pd.read_csv(file,sep ='\t')
    print(df.head())
    return df


def parse_v2(df,f):
    df['sample_id'] = f.replace(".tsv","")
    df['amino_acid'] = df['aminoAcid']
    df['templates'] = df['count (templates/reads)']
    df['rearrangement'] = df['nucleotide']
    df['frequency'] = df['frequencyCount (%)']
    df['total_rearrangements'] = df['templates'].sum()
    df['frame_type'] = df['sequenceStatus']
    df['v_family'] = df['vFamilyName']
    df['v_gene'] = df['vGeneName']
    df['j_gene'] = df['jGeneName']
    df = df.query('frame_type == "In"').reset_index()
    df['v'] = df['v_family'].str.replace("TCRB","")
    df['productive_frequency'] = df['templates'] / df['templates'].sum()
    ix1 = df['amino_acid'].str.len() >= 6
    ix2 = df['amino_acid'].notna()
    df= df[ix1&ix2].reset_index()
    dfout = df[['v','templates','frequency','productive_frequency','v_family','v_gene','j_gene', 'amino_acid','rearrangement','total_rearrangements']].\
    sort_values('templates', ascending = False)
    return(dfout)

def strat_v2_single(f, zfile, outdir, parse_func = parse_v2, sep = "\t"):
    z = zipfile.ZipFile(os.path.join(zfile))
    file = z.open(f)
    df = pd.read_csv(file,sep =sep)
    df = parse_func(df,f)
    for v,g in df.groupby('v'):
        name_without_tsv = os.path.basename(f).replace(".tsv","").replace(".csv","")
        g['sample_id'] = name_without_tsv
        new_name = f"{v}_{name_without_tsv}.csv"
        g.to_csv(os.path.join(outdir, v, new_name),index = False)
    return True

def vfamcdr3_v2_single(f, zfile, outdir, parse_func = None, sep = "\t"):
    z = zipfile.ZipFile(os.path.join(zfile))
    file = z.open(f)
    df = pd.read_csv(file, sep =sep, low_memory = False)
    df = parse_func(df,f)
    name_without_tsv = os.path.basename(f).replace(".tsv","").replace(".csv","")
    new_name = f"{name_without_tsv}.csv"
    df.to_csv(os.path.join(outdir, new_name),index = False)
    return True


def combine_to_dest(x):
    folder = x[0]
    dest   = x[1]  
    if isinstance(folder, str):
        if os.path.isdir(folder):
            fs = os.listdir(folder)
            s = list()
            for f in fs:
                 s.append(pd.read_csv(os.path.join(folder, f)))
            if len(s) > 0:
                dfv = pd.concat(s).reset_index(drop = True)
                ix1 = dfv['amino_acid'].str.len() >= 6
                ix2 = dfv['amino_acid'].notna()
                dfv = dfv[ix1&ix2].reset_index(drop = True)
                dfv.to_csv(dest, index = False)
            #print(f"Combined {folder} ---> {dest})")
    return True

def combine_to_dest2(x):
    folder = x[0]
    dest   = x[1] 
    checklist = x[2]
    if isinstance(folder, str):
        if os.path.isdir(folder):
            fs = os.listdir(folder)
            print("subsetting to checklist")
            v = fs[0].split("_")[0]
            fs = [x for x in fs if x.replace(f"{v}_","") in checklist ]
            print(f"Length of {len(fs)} after using checklist")
            s = list()
            for f in fs:
                 s.append(pd.read_csv(os.path.join(folder, f)))
            if len(s) > 0:
                dfv = pd.concat(s).reset_index(drop = True)
                ix1 = dfv['amino_acid'].str.len() >= 6
                ix2 = dfv['amino_acid'].notna()
                dfv = dfv[ix1&ix2].reset_index(drop = True)
                dfv.to_csv(dest, index = False)
            #print(f"Combined {folder} ---> {dest})")
    return True

def combine_to_dest_filelist(x, filelist):
    folder = x[0]
    dest   = x[1]  
    v = os.path.basename(folder)
    filelist = [f"{v}_{x}.csv" for x in filelist]
    print(filelist)
    if isinstance(folder, str):
        if os.path.isdir(folder):
            fs = os.listdir(folder)
            fs = [f for f in fs if f in filelist]
            s = list()
            for f in fs:
                 s.append(pd.read_csv(os.path.join(folder, f)))
            dfv = pd.concat(s).reset_index(drop = True)
            dfv.to_csv(dest, index = False)
            #print(f"Combined {folder} ---> {dest})")
    return True

def read_and_tag(x, tag_col = "V"):
    tag = os.path.basename(x).split(".")[0]
    dx = pd.read_csv(x)
    dx[tag_col] = tag 
    return dx


from collections import Counter
def subsample_repertoires(df, n_sample=20_000, seed=1, replace = True):
    """
    Subsamples TCR repertoires based on template counts, preserving productive frequency.

    Parameters:
    - df (pd.DataFrame): Input DataFrame with a 'templates' column.
    - n_sample (int): Number of samples to draw.
    - seed (int): Random seed for reproducibility.

    Returns:
    - pd.DataFrame: Subsampled DataFrame with adjusted template counts and frequencies.
    """
    # Expand index based on 'templates' counts
    repeated_index = df.index.repeat(df['templates'].values)

    # Randomly sample indices
    rng = np.random.default_rng(seed)
    sampled_indices = rng.choice(repeated_index, size=n_sample, replace=replace)

    # Count occurrences in the sample
    sampled_counts = Counter(sampled_indices)
    df_s = pd.DataFrame(sampled_counts.items(), columns=['index', 'templates'])

    # Compute productive frequency
    df_s['productive_frequency'] = df_s['templates'] / df_s['templates'].sum()

    # Preserve metadata from original df
    df_s['index'] = df_s['index'].astype(int)  # Ensure it's an integer index
    df_s = df_s.set_index('index')

    # Merge with original DataFrame to retrieve all columns
    df_s = df_s.join(df, how='left', rsuffix='_presample')
    df_s = df_s.sort_values('templates', ascending = False)

    # Reset index and drop original index column
    df_s.reset_index(drop=True, inplace=True)

    return df_s

def subsample_vfamcdr3(fp, outdir_vfamcdr3, outdir_vfamcdr3_subsample, n_samples, seed_i=0):
    """
    Reads a TCR dataset, performs subsampling at different sizes, and saves results.

    Parameters:
    - fp (str): File path to the input dataset.
    - outdir_vfamcdr3 (str): Directory containing input files.
    - outdir_vfamcdr3_subsample (str): Directory to save subsampled outputs.
    - n_samples (list): List of sample sizes to process.
    - seed_i (int): Seed for reproducibility.
    """
    df = pd.read_csv(os.path.join(outdir_vfamcdr3, fp))  
    name = os.path.splitext(fp)[0]  # Remove file extension

    results = {}
    for n_sample in n_samples:
        new_name = f"{name}__{n_sample}__{seed_i}"
        print(f"Processing: {new_name}")

        # Perform subsampling
        t = subsample_repertoires(df, n_sample=n_sample, seed=seed_i)

        # Store results in dictionary
        results[new_name] = t

        # Save the output
        new_name_csv = f"{new_name}.csv"
        t[['vfamcdr3', 'templates']].to_csv(
            os.path.join(outdir_vfamcdr3_subsample, new_name_csv), index=False
        )

    return results  # Returning results can be useful for further analysis

# This is for purpose of getting number above some thrshold
def get_dfu_info_above_min_val(file_path,   get_col = "productive_frequency", min_value = 2E-6, sep = None):
    if sep is None:
        if file_path.endswith(".csv"):
            sep = ","
        elif file_path.endswith(".tsv"):
            sep = "\t"
        else:
            raise ValueError("File must be a .csv or .tsv file")

    d = pd.read_csv(file_path, sep = sep)
    d2 = d[d[get_col] > min_value]
    i = os.path.basename(file_path).replace(".csv","").replace(".tsv","")
    return (i, d.productive_frequency.min(), d2.productive_frequency.min(), d.shape[0], d2.shape[0])


def get_revelant_binaries(subject_binary_file, min_allele_prev = .1):
    b = pd.read_csv(subject_binary_file, sep = "\t", index_col = 0)
    ap = b.sum()/b.shape[0]
    return ",".join(sorted(ap[ap > min_allele_prev].\
        sort_values(ascending = False).\
        index.to_list()))