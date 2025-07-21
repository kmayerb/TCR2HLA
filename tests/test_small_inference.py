import argparse
import sys
import os 
import pandas as pd 
import numpy as np
import tcrtest
from tcrtest.classify import HLApredict
from tcrtest.classify import map_allele2
from tcrtest.ui import VfamCDR3
#/home/kmayerbl/miniconda3/envs/tcrdist311/bin/pytest . -v -s

def test_basic_foward_precition():
    """
    This is a simple test of the forward inference.
    1. Parse files 
    2. Create occurance matrix
    3. Choose model
    4. Perform linear combination of features and weights
    5. Apply calibration
    6. Output results
    """
    args = argparse.Namespace()
    args.zipfile = 'towlerton25.zip'
    args.name = 'towlerton25_demo'
    args.cpus = 2
    args.project_folder = "demo"
    args.get_col = "productive_frequency"
    args.on = "vfamcdr3"
    args.min_value = 2E-6
    args.model_name = "XSTUDY_ALL_FEATURE_L1_v4e"
    args.calibration_name = "XSTUDY_ALL_FEATURE_L1_v4e_HS2"


    v = VfamCDR3(
        project_folder   = args.project_folder,
        input_zfile      = args.zipfile,
        input_gz_file    = None,
        cpus = args.cpus)
    zfs = v.get_raw_files()[0:2]
    v.parse_adaptive_files(checklist = zfs)
    assert os.path.isdir(v.outdir_vfamcdr3)
    fs = [x for x in os.listdir(v.outdir_vfamcdr3) if x.endswith('csv') or x.endswith('tsv')]
    filelist = [os.path.join(v.outdir_vfamcdr3, x) for x in fs ]
    fs_df = pd.DataFrame({'filename':fs})
    package_folder = os.path.dirname(tcrtest.__file__)
    model_name = args.model_name 
    model_folder = os.path.join(package_folder,'models', model_name)
    print(model_folder)
    query_file = os.path.join(model_folder,f"{model_name}.query.csv")
    query_df = pd.read_csv(os.path.join(model_folder,f"{model_name}.query.csv"), index_col = 0)
    project_folder   = args.project_folder
    X, I = v.build_feature_occurance_matrix(
            query_df = query_df,
            filelist = filelist ,
            get_col=args.get_col,
            min_value=args.min_value,
            on=args.on,
            cpus=args.cpus,
            add_counts=False)
    Xt = (X > 0).astype(int).transpose()
    h = HLApredict(Q = query_df)
    h.load_fit(model_folder = model_folder, model_name = args.model_name)    
    h.load_calibrations(model_folder =model_folder,
                model_name = args.calibration_name ) 
    h.predict_decisions_x(Xt)
    if args.calibration_name.endswith("_HS2"):
        h.pxx(h.decision_scores, covariates = I[['log10unique','log10unique2']] )
    else: 
        h.pxx(h.decision_scores, covariates = None)
    
    print(h.calibrated_prob)

    #h.calibrated_prob.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_calibrated_probs.csv"), index = False)
    #h.decision_scores.to_csv(os.path.join(v.project_folder, f"samples_{args.name}_x_decision_scores.csv"), index = False)
    result = h.output_probs_and_obs(probs = h.calibrated_prob, observations = h.calibrated_prob > .5 )
    result['locus'] = result['binary'].apply(lambda x : map_allele2(x))
    result = result.rename(columns = {'obs':'pred'})
    result = result.merge(I[['log10unique','log10unique2']], how = "left", left_on = "sample_id", right_on = 'sample_id')
    
    print(result)

    assert np.all(result.columns == ['p', 'pred', 'sample_id', 'binary', 'locus', 'log10unique','log10unique2'])