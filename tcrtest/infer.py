#!/usr/bin/env python3
import argparse
import sys
import os 
import pandas as pd 
import numpy as np
import zipfile


try:
    import tcrtest
    from tcrtest.classify import HLApredict
    from tcrtest.classify import map_allele2
    from tcrtest.ui import VfamCDR3
except ModuleNotFoundError as e:
    print(
        "\n[WARNING] Could not import tcrtest modules. "
        "This usually means you are running locally without installing the package.\n"
        "Temporarily adding '/fh/fast/gilbert_p/kmayerbl/TCR2HLA' to your Python path.\n"
        f"Original error: {e}\n"
    )
    sys.path.insert(0, "/fh/fast/gilbert_p/kmayerbl/TCR2HLA")
    import tcrtest
    from tcrtest.classify import HLApredict
    from tcrtest.classify import map_allele2
    from tcrtest.ui import VfamCDR3


def detect_file_format(df):
    """
    Autodetects file format based on DataFrame columns.
    Returns 'adaptive_v2' or 'mixcr'.
    Raises ValueError if format cannot be determined.
    """
    adaptive_v2_cols = {'vMaxResolved','jMaxResolved', 'aminoAcid', 'count (templates/reads)','frequencyCount (%)'}
    mixcr_cols       = {'allVHitsWithScore ', 'allJHitsWithScore', 'aaSeqCDR3','uniqueMoleculeCount', 'uniqueMoleculeFraction'}

    df_cols = set(df.columns)

    if adaptive_v2_cols.issubset(df_cols):
        return 'adaptive_v2'
    elif mixcr_cols.issubset(df_cols):
        return 'mixcr'
    else:
        raise ValueError("Unknown file format: columns do not match known formats.")

def detect_file_format_in_zip(zip_path):
    """
    Given a path to a zipfile, find the first .csv or .tsv file,
    read only the first row as a DataFrame, and apply detect_file_format().
    Returns the detected format as a string.
    """
    with zipfile.ZipFile(zip_path, 'r') as z:
        file_list = z.namelist()
        for fname in file_list:
            if fname.endswith('.csv') or fname.endswith('.tsv'):
                with z.open(fname) as f:
                    if fname.endswith('.csv'):
                        df = pd.read_csv(f, nrows=1)
                    else:
                        df = pd.read_csv(f, sep='\t', nrows=1)
                return detect_file_format(df)
        raise FileNotFoundError("No .csv or .tsv file found in the zip archive.")


def main(args = None):
    """
    Main entry point for running HLA inference from TCR sequencing data.

    This function parses input arguments, handles data loading (from zip or folder),
    builds an occurrence matrix, loads pretrained models, runs predictions,
    performs optional calibration, and outputs results to CSV files.

    Parameters
    ----------
    args : list of str, optional
        Command-line arguments to parse. If None, arguments are taken from sys.argv.

    Returns
    -------
    None
    """
    # Create a parser with a simple description
    parser = argparse.ArgumentParser(
        description="Process either a zipfile or an input folder, but not both."
    )
    
    parser.add_argument(
        "--truth_values",
        type=str,
        default = None,
        help="Path to the input folder to be processed."
    )

    # parse.add_argument('--occurrence_matrix',
    #     default = False,
    #     type='store_true',
    #     help="resuse previously computed occurrence matrix")

    parser.add_argument(
        "--force",
        default = False,
        action = "store_true",
        help="")

    # int arguments
    parser.add_argument(
        "--cpus",
        default = 2,
        type=int,
        help="")
    # String argument
    parser.add_argument(
        "--name",
        required = True,
        type=str,
        help="")
    parser.add_argument(
        "--project_folder",
        type = str,
        default = "demo_project",
        help="e.g. '/fh/fast/gilbert_p/kmayerbl/mitchell_classifier_eval/'"
    )

    parser.add_argument(
        "--get_col",
        type=str,
        default='productive_frequency',
        help="Argument passed to : VfamCDR3.build_feature_occurance_matrix() "
    )

    parser.add_argument(
        "--on",
        type=str,
        default='vfamcdr3',
        help="Argument passed to : VfamCDR3.build_feature_occurance_matrix() "
    )

    parser.add_argument(
        "--model_name",
        type=str,
        default='XSTUDY_ALL_FEATURE_L1_v4e',
        help=""
    )
    parser.add_argument(
        "--calibration_name",
        type=str,
        default='XSTUDY_ALL_FEATURE_L1_v4e_HS2',
        help=""
    )

    parser.add_argument(
        "--test_mode",
        action = 'store_true',
        default=False,
        help=""
    )

    # Float Arguments
    parser.add_argument(
        "--min_value",
        default = 2E-6,
        type=float,
        help="Smallest frequency to consider for occurrence_matrix")
    # Store True Arguments
    parser.add_argument(
        "--download_towlerton_zip",
        action= "store_true",
        default=False,
        help="For demonstration purpose, download a dataset"
    )
    parser.add_argument(
        "--parse_adaptive_files",
        action= "store_true",
        default=False,
        help="Assumes adaptive V2 format will parse producitve clones to vfamcdr3 format"
    )
    parser.add_argument(
        "--sep",
        type=str,
        default = ",",
        help="input folder separator, default is ','"
    )

    parser.add_argument(
        "--gate1",
        type=float,
        default = .5,
        help="gate1 for HLA inference, default is .5"
    )

    parser.add_argument(
        "--gate2",
        type=float,
        default = .5,
        help="gate2 for HLA inference, default is .5"
    )
    # Define a mutually exclusive group that requires exactly one argument
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--zipfile",
        type=str,
        help="Path to the zip file to be processed."
    )
    group.add_argument(
        "--input_folder",
        type=str,
        default = None,
        help="Path to the input folder to be processed."
    )

    
    # Parse the command line arguments
    args = parser.parse_args(args)

    if args.sep == r"\t" or args.sep == "\\t":
        args.sep = "\t"

    print(f"\t-------------------------------------------------------")
    print("\tArguments:")
    for arg, value in vars(args).items():
        print(f"\t\t{arg}: {value}")
    print(f"\t-------------------------------------------------------")
    
    """
    For demonstration purpose we have flag that downloads the full towlerton dataset
    """
    if args.download_towlerton_zip:
        v = VfamCDR3(
             project_folder   = args.project_folder,
             input_zfile      = None,
             input_gz_file    = None,
             cpus = args.cpus)
        print(f"\t-------------------------------------------------------")
        print(f"\t <download_towlerton_zip> was set to true ")
        print(f"\tFor demonstration purposes you've downloaded a zip file.")
        print(f"\t-------------------------------------------------------")
        import subprocess
        from pathlib import Path
        towlerton_zip_path = os.path.join(args.project_folder,'towlerton.zip')
        url = 'https://www.dropbox.com/scl/fi/k1pz9m4jtl0gg0yc8nyg8/sampleExport.2024-02-29_20-38-59.zip?rlkey=z2ebf4c963rez4l46rvmlaw7d&st=yf24ehko&dl=1'
        # Use subprocess to run wget command
        cmd = ["wget", "-O", str(towlerton_zip_path), url]
        print('Executing command:', ' '.join(cmd))
        subprocess.run(cmd, check=True)
        #import sys; sys.exit()


    """
    User may have selected a zip file
    """
    if args.zipfile:
        print("\tZip file provided:", args.zipfile)
        assert os.path.isfile(args.zipfile)
        print("\tUsing zip file provided:", args.zipfile)
        assert os.path.isfile(args.zipfile)
        
        # --- Format detection and auto-parse logic ---
        try:
            detected_format = detect_file_format_in_zip(args.zipfile)
            print(f"\tDetected file format in zip: {detected_format}")
            if detected_format == "adaptive_v2":
                args.parse_adaptive_files = True
                print("\tAuto-enabled --parse_adaptive_files based on detected format (adaptive_v2).")
            else:
                raise ValueError("Only 'adaptive_v2' format is supported for automatic parsing." \
                "Please manually extract and parse your files, then use the --input_folder option.")
        except Exception as e:
            raise RuntimeError(
                f"Could not automatically detect a supported file format in the zip archive.\n"
                f"Error: {e}\n"
                f"Please manually extract and parse your files, then use the --input_folder option."
            )

        v = VfamCDR3(
            project_folder   = args.project_folder,
            input_zfile      = args.zipfile,
            input_gz_file    = None,
            cpus = args.cpus)
        """
        Note: If files are adaptive v2 format, we can parse them into concise search 
        files, otherwise user can parse files themselves and provide and 
        <input_folder>
        """

        if args.parse_adaptive_files:
            print(f"\t-------------------------------------------------------")
            print(f"\tParsing Adaptive formated files to:")
            print(f"\t{v.outdir_vfamcdr3}")
            print(f"\t-------------------------------------------------------")
            if args.test_mode:
                zfs = v.get_raw_files()[0:10]
                v.parse_adaptive_files(checklist = zfs)
            else:
                v.parse_adaptive_files()


        assert os.path.isdir(v.outdir_vfamcdr3)
        fs = [x for x in os.listdir(v.outdir_vfamcdr3) if x.endswith('csv') or x.endswith('tsv')]
        filelist = [os.path.join(v.outdir_vfamcdr3, x) for x in fs ]
        if args.test_mode:
            filelist = filelist[0:4]
        fs_df = pd.DataFrame({'filename':fs})
        print(f"\t-------------------------------------------------------")
        print(f"\tzipfile contained: ")
        print(f"\t{len(filelist)} files recognized.")
        print(f"\t-------------------------------------------------------")

    elif args.input_folder:
        print("Input folder provided:", args.input_folder)
        assert os.path.isdir(args.input_folder)
        fs = [x for x in os.listdir(args.input_folder) if x.endswith('csv') or x.endswith('tsv')]
        filelist = [os.path.join(args.input_folder, x) for x in fs ]
        if args.test_mode:
            filelist = filelist[0:4]
        fs_df = pd.DataFrame({'filename':fs})
        print(f"\t-------------------------------------------------------")
        print(f"\tYou provided an <input_folder> argument.")
        print(f"\t{args.input_folder}")
        print(f"\t{len(filelist)} files recognized.")
        print(f"\t-------------------------------------------------------")
        print(fs_df.head())


        v = VfamCDR3(
            project_folder   = args.project_folder,
            input_zfile      = None,
            input_gz_file    = None,
            cpus = args.cpus)
        print(f"\t-------------------------------------------------------")
        print(f"\tUser provided an <input_folder> argument.")
        print(f"\tSetting VfamCDR3.outdir_vfamcdr3 to {args.input_folder}")
        print(f"\t-------------------------------------------------------")
        v.outdir_vfamcdr3 = args.input_folder

    else:
        print("No <zipfile> or <input_folder> argument provided")
        import sys; sys.exit()
    

    """
    Now we load specified model
    """
#
    package_folder = os.path.dirname(tcrtest.__file__)
    model_name = args.model_name 
    model_folder = os.path.join(package_folder, 'models', model_name)
    print(model_folder)
    query_file = os.path.join(model_folder,f"{model_name}.query.csv")
    query_df = pd.read_csv(os.path.join(model_folder,f"{model_name}.query.csv"), index_col = 0)
    project_folder   = args.project_folder 

    print(f"\t-------------------------------------------------------")
    print(f"\tSearching for HLA-associated anchor features in {len(filelist)} files.")
    print(f"\tSpeed will depend on <cpus> argument (current cpus: {args.cpus}).")
    print(f"\t-------------------------------------------------------")
    
    """Check to see if you alread computed an occurence matrix"""
    if os.path.isfile(os.path.join(v.project_folder, f"query_x_{args.name}.npz")):
        assert os.path.isfile(os.path.join(v.project_folder,f"samples_x_{args.name}.csv"))
        assert os.path.isfile(os.path.join(v.project_folder, f"query_x_{args.name}.npz.columns.csv"))
        print(f"\t-------------------------------------------------------")
        print(f"\tOccurence matrix alrady detected. <force> set to : {args.force}")
        if args.force: 
            print(f"\tRecomputing occurence matrix")
            print(f"\t-------------------------------------------------------")
            X,I = v.build_feature_occurance_matrix(
                query_df = query_df,
                filelist = filelist ,
                get_col=args.get_col,
                min_value=args.min_value,
                sep = args.sep,
                on=args.on,
                cpus=args.cpus,
                add_counts=False)
            #import pdb; pdb.set_trace()
        else:
            print(f"\tUsing existing matrix")
            print(f"\t-------------------------------------------------------")
            X = v.load_occurrence_matrix(indir =v.project_folder, infile = f"query_x_{args.name}.npz")
            I = pd.read_csv(os.path.join(v.project_folder, f"samples_x_{args.name}.csv"), index_col = 0)
     
    else:
        X, I = v.build_feature_occurance_matrix(
                query_df = query_df,
                filelist = filelist ,
                get_col=args.get_col,
                min_value=args.min_value,
                sep = args.sep,
                on=args.on,
                cpus=args.cpus,
                add_counts=False)
        #import pdb; pdb.set_trace()
        # temporary solution 
        X = X.loc[query_df.index]
        print(f"\t-------------------------------------------------------")
        print(f"\tWriting occurance matrix")# query_x_{args.name}.npz to {v.project_folder}.")
        print(f"\t-------------------------------------------------------")
        v.save_occurrence_matrix(X, outdir = v.project_folder, outfile = f"query_x_{args.name}.npz")
        print(f"\t-------------------------------------------------------")
        print(f"\tWriting samples_x_{args.name}.csv to {v.project_folder}.")
        print(f"\t-------------------------------------------------------")
        I.to_csv(os.path.join(v.project_folder, f"samples_x_{args.name}.csv"))

    print(f"\t-------------------------------------------------------")
    print(f"\tPerforming HLA inference with")
    print(f"\tModel name        : {args.model_name}")
    print(f"\tModel calibration : {args.calibration_name}")
    print(f"\t-------------------------------------------------------")
    Xt = (X > 0).astype(int).transpose()

    h = HLApredict(Q = query_df)
    h.load_fit(model_folder = model_folder,
               model_name = args.model_name)
    
    
    h.load_calibrations(model_folder =model_folder,
               model_name = args.calibration_name ) #<-- Note here we use (HS2) because of low sequencing depth of these samples
    # For a calibrated probability you need information on # of unique clones
    h.predict_decisions_x(Xt)
    if args.calibration_name.endswith("_HS2"):
        h.pxx(h.decision_scores, covariates = I[['log10unique','log10unique2']] )
    else: 
        h.pxx(h.decision_scores, covariates = None)
    # Automatically output the calibrated probs
    print(f"\t-------------------------------------------------------")
    print(f"\t Writing -- {v.project_folder}/samples_{args.name}_x_calibrated_probs.csv/.tsv")
    print(f"\t-------------------------------------------------------")
    print(h.calibrated_prob)
    h.calibrated_prob.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs.csv"), index = True)
    h.calibrated_prob.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs.tsv"), sep = "\t", index = True)
    (h.calibrated_prob > 0.5).astype('boolean').to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs_boolean.tsv"))

    h.decision_scores.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_decision_scores.csv"), index = True)
    result = h.output_probs_and_obs(probs = h.calibrated_prob, observations = h.calibrated_prob > .5 )

    # --- New: Mask values > gate1 and < gate2 as NA and output to a new file ---
    gated_probs = h.calibrated_prob.copy()
    mask = (gated_probs > args.gate1) & (gated_probs < args.gate2)
    gated_probs = gated_probs.mask(mask)
    gated_probs.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs_gated.csv"), index=True)
    gated_probs.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs_gated.tsv"), sep = '\t', index=True)
    gated_probs_boolean = gated_probs.apply(lambda col: np.where(np.isnan(col), pd.NA, col > 0.5)).astype('boolean')
    #import pdb; pdb.set_trace() # SEE CRITICAL LINE BELOW
    gated_probs_boolean.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs_gated_boolean.csv"), index=True)
    gated_probs_boolean.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs_gated_boolean.tsv"), sep = '\t', index=True)

    print(f"\t-------------------------------------------------------")
    print(f"\t Writing -- {v.project_folder}/samples_{args.name}_x_calibrated_probs_gated.csv/.tsv")
    print(f"\t-------------------------------------------------------")
    print(gated_probs)


    print(f"\t-------------------------------------------------------")
    print(f"\t Writing -- {v.project_folder}/samples_{args.name}_x_calibrated_probs_gated_boolean.csv/.tsv")
    print(f"\t-------------------------------------------------------")
    print(gated_probs_boolean)
    #import pdb; pdb.set_trace() # S

    print(f"\t-------------------------------------------------------")
    print(f"\t Writing -- {v.project_folder}/samples_{args.name}_predictions.long.csv")
    print(f"\t-------------------------------------------------------")
    result['locus'] = result['binary'].apply(lambda x : map_allele2(x))
    result = result.rename(columns = {'obs':'pred'})
    result = result.merge(I[['log10unique','log10unique2']], how = "left", left_on = "sample_id", right_on = 'sample_id')
    result['locus'] = result['binary'].apply(lambda x : map_allele2(x))
    result['model'] = args.model_name
    result['calibration'] = args.calibration_name
    result['name'] = args.name

    result.to_csv(os.path.join(v.project_folder, f"sample_x_{args.name}_predictions.long.csv"), index = False)
    print(f"\t-------------------------------------------------------")
    print(f"\t Showing positive preditions. All values provided in .CSV")
    print(f"\t-------------------------------------------------------")
    print(result.sort_values(['sample_id','locus']).query('pred == True'))
    
    
    if isinstance(args.truth_values, str):
        assert os.path.isfile(args.truth_values)
        if args.truth_values.endswith('.csv'):
            sep = ','
        elif args.truth_values.endswith('.tsv'):
            sep = '\t'
        Y = pd.read_csv(args.truth_values, sep = sep, index_col = 0)
        P = h.calibrated_prob
        Y = Y.loc[P.index]
        
        result2 = h.output_probs_and_obs(probs = h.calibrated_prob, observations = Y.astype('float64'))
        result2['pred'] = (result2['p'] > .5).astype('float64')
        result2['locus'] = result2['binary'].apply(lambda x : map_allele2(x))
        result2 = result2[['p','pred','obs','sample_id','binary','locus']]
        result2 = result2.merge(I[['log10unique','log10unique2']], how = "left", left_on = "sample_id", right_on = 'sample_id')
        
        result2['locus'] = result2['binary'].apply(lambda x : map_allele2(x))
        result2['model'] = args.model_name
        result2['calibration'] = args.calibration_name
        result2['name'] = args.name
      
  
        print(f"\t-------------------------------------------------------")
        print(f"\t Writing -- {v.project_folder}/samples_{args.name}_predictions.observations.long.csv")
        print(f"\t-------------------------------------------------------")
        result2.to_csv(os.path.join(v.project_folder, f"sample_x_{args.name}_predictions.observations.long.csv"), index = False)
        print(f"\t-------------------------------------------------------")
        print(f"\t Showing positive preditions. All values provided in .CSV")
        print(f"\t-------------------------------------------------------")
        print(result2.sort_values(['sample_id','locus']).query('pred == True'))

        result3 = h.score_predictions(probs = h.calibrated_prob, observations = Y.astype('float64'), gate = (args.gate1, args.gate2))
        #result3['binary'] = result3['i']
        #result3.drop(columns = ['i'], inplace = True)
        result3['locus'] = result3['binary'].apply(lambda x : map_allele2(x))
        result3['model'] = args.model_name
        result3['calibration'] = args.calibration_name
        result3['name'] = args.name
        print(f"\t-------------------------------------------------------")
        print(f"\t Writing -- {v.project_folder}/sample_x_{args.name}_performance.csv")
        print(f"\t-------------------------------------------------------")
        result3.to_csv(os.path.join(v.project_folder, f"sample_x_{args.name}_performance.csv"), 
                       index = False)
        print(result3)

if __name__ == "__main__":
    main()




"""
#Test example1

TCR2HLA \
  --model_name XSTUDY_ALL_FEATURE_L1_v4 \
  --calibration_name XSTUDY_ALL_FEATURE_L1_v4_HS2 \
  --name demo1 \
  --project_folder demo1_output \
  --zipfile towlerton25.zip \
  --cpus 2 \
  --test_mode

"""










# """README.md 



# import pandas as pd
# f = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4/XSTUDY_ALL_FEATURE_L1_v4.query.csv'
# d = pd.read_csv(f,index_col = 0)
# #d[~d.search.isna()].to_csv(f)


# # ADD BACK NUMERICAL INDEX
# import pandas as pd
# f = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4e/XSTUDY_ALL_FEATURE_L1_v4e.query.csv'
# d = pd.read_csv(f)
# # d.to_csv( '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4e/XSTUDY_ALL_FEATURE_L1_v4e.query.csv')
# # import pandas as pd
# # f = '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v7e/XSTUDY_ALL_FEATURE_L1_v7e.query.csv'
# # d = pd.read_csv(f)
# # d.to_csv( '/fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v7e/XSTUDY_ALL_FEATURE_L1_v7e.query.csv')




# Examples
# python infer.py --name testtow \
#     --download_towlerton_zip \
#     --project_folder /fh/fast/gilbert_p/kmayerbl/tow_ex/ \
#     --cpus 24 \
#     --input_folder /fh/fast/gilbert_p/kmayerbl/large_cohorts/rosati/mixcr_parsed/healthy_test\
#     --model_name XSTUDY_ALL_FEATURE_L1_v4 \
#     --calibration_name XSTUDY_ALL_FEATURE_L1_v4_HS2

# python infer.py --name testtow \
#     --force \
#     --zipfile /fh/fast/gilbert_p/kmayerbl/tow_ex/towlerton.zip \
#     --project_folder /fh/fast/gilbert_p/kmayerbl/tow_ex/ \
#     --cpus 24 \
#     --model_name XSTUDY_ALL_FEATURE_L1_v4 \
#     --calibration_name XSTUDY_ALL_FEATURE_L1_v4_HS2 \
#     --truth_values /fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4/sample_hla_x_towlerton.csv

# python infer.py --name testtow \
#     --zipfile /fh/fast/gilbert_p/kmayerbl/tow_ex/towlerton.zip \
#     --project_folder /fh/fast/gilbert_p/kmayerbl/tow_ex/ \
#     --cpus 24 \
#     --model_name XSTUDY_ALL_FEATURE_L1_v4 \
#     --calibration_name XSTUDY_ALL_FEATURE_L1_v4_HS2 \
#     --truth_values /fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4/sample_hla_x_towlerton.csv

# """


# """
# python tcrtest/tcrtest/infer.py \
#   --name demo7 \
#   --model_name XSTUDY_ALL_FEATURE_L1_v7 \
#   --calibration_name XSTUDY_ALL_FEATURE_L1_v7 \
#   --parse_adaptive_files \
#   --zipfile ./demo_project/towlerton25.zip \
#   --project_folder ./demo_project/ \
#   --cpus 24
  
# python tcrtest/tcrtest/infer.py \
#   --name demo4 \
#   --model_name XSTUDY_ALL_FEATURE_L1_v4e \
#   --calibration_name XSTUDY_ALL_FEATURE_L1_v4e_HS2 \
#   --parse_adaptive_files \
#   --zipfile ./demo_project/towlerton25.zip \
#   --project_folder ./demo_project/ \
#   --cpus 24 \
#   --force \
#   --truth_values /fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4/sample_hla_x_towlerton.csv

  
# python tcrtest/tcrtest/infer.py \
#   --name demo4 \
#   --model_name XSTUDY_ALL_FEATURE_L1_v4e \
#   --calibration_name XSTUDY_ALL_FEATURE_L1_v4e_HS2 \
#   --parse_adaptive_files \
#   --zipfile ./demo_project/towlerton.zip \
#   --project_folder ./demo_project/ \
#   --cpus 24 \
#   --force \
#   --truth_values /fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4/sample_hla_x_towlerton.csv

  
# python tcrtest/tcrtest/infer.py \
#   --name demo7e \
#   --model_name XSTUDY_ALL_FEATURE_L1_v7e \
#   --calibration_name XSTUDY_ALL_FEATURE_L1_v7e \
#   --parse_adaptive_files \
#   --zipfile ./demo_project/towlerton.zip \
#   --project_folder ./demo_project/ \
#   --cpus 24 \
#   --force \
#   --truth_values /fh/fast/gilbert_p/kmayerbl/tcrtest/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v4/sample_hla_x_towlerton.csv


# python tcrtest/tcrtest/infer.py \
#   --name demo4 \
#   --model_name XSTUDY_ALL_FEATURE_L1_v4e \
#   --calibration_name XSTUDY_ALL_FEATURE_L1_v4e_HS2 \
#   --parse_adaptive_files \
#   --zipfile ./demo_project/towlerton25.zip \
#   --project_folder ./demo_project/ \
#   --cpus 24
# """