import os
import pandas as pd
import numpy as np
import parmap
from functools import partial

def parse_v2x_gt1(df,f, gt1= False, out_cols = ['v','vfamcdr3','templates','frequency','productive_frequency','v_family','v_gene','j_gene','v_resolved','amino_acid','rearrangement','total_rearrangements','sample_id']):
    cols = df.columns
    # THIS IS FOR "V2" ADAPTIVE COLUMNS
    if 'aminoAcid' in cols:
        df['amino_acid'] = df['aminoAcid']
    if 'count (templates/reads)' in cols:
        df['templates'] = df['count (templates/reads)']
    if 'nucleotide' in cols:
        df['rearrangement'] = df['nucleotide']
    if 'frequencyCount (%)' in cols:
        df['frequency'] = df['frequencyCount (%)']
    if 'sequenceStatus' in cols:
        df['frame_type'] = df['sequenceStatus']
    if 'vFamilyName' in cols:
        df['v_family'] = df['vFamilyName']
    if 'vGeneName' in cols:
        df['v_gene'] = df['vGeneName']
    if 'jGeneName' in cols:
        df['j_gene'] = df['jGeneName']
    if 'vMaxResolved' in cols:
        df['v_resolved'] = df['vMaxResolved']
    if 'jMaxResolved' in cols:
        df['j_resolved'] = df['jMaxResolved']


    if 'v_resolved' not in cols:
        df['v_resolved'] = df['v_gene'].replace('unresolved', np.nan)
        df['v_resolved'] = df['v_resolved'].fillna(df['v_family'])
    
    if 'frequency' not in cols:
        df['frequency'] = np.nan
    if 'rearrangement' not in cols:
        df['rearrangement'] = np.nan

    for x in ['amino_acid', 'templates',  'v_gene', 'j_gene', 'v_family', 'v_resolved','frequency']:
       assert x in df.columns, f"{x} is missing"

    df['v'] = df['v_resolved'].apply(lambda x: x.split("*")[0].split("-")[0].replace("TCRB","") if isinstance(x,str) else np.nan)
    #df['v'] = df['v_family'].str.replace("TCRB","")
    if 'frame_type' in cols:
        df = df.query('frame_type == "In"').reset_index()
    
    df['productive_frequency'] = df['templates'] / df['templates'].sum()
    
    ix1 = df['amino_acid'].str.len() >= 6
    ix2 = df['amino_acid'].notna()
    df= df[ix1&ix2].reset_index(drop = True)
    ix3 = df['amino_acid'].apply(lambda x: x.find("*") == -1 )
    df= df[ix3].reset_index(drop = True)


    df = df[df['v'] != "unresolved"].reset_index(drop = True)
    df = df[df['v'] != "unknown"].reset_index(drop = True)
    df = df[df['v'] != np.nan].reset_index(drop = True)
    df['V'] = df['v']

    df['total_rearrangements'] = df.shape[0]

    if gt1:
        df= df[df['templates'] > 1].reset_index(drop = True)

    df['sample_id'] = os.path.basename(f).replace(".tsv","").replace('.csv','')
    df['vfamcdr3'] = df['v']+df['amino_acid']
    #print(os.path.basename(f).replace(".tsv","").replace('.csv',''))

    dfout = df[ out_cols].sort_values('templates', ascending = False)
    return(dfout)

from functools import partial
parse_v2x_gt1_vfamcdr3 =  partial(parse_v2x_gt1, gt1= False, out_cols = ['vfamcdr3','templates','productive_frequency','V'])