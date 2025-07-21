import os 
import pandas as pd 
import re

def to_imgt_format(allele_str):
    """
    Convert an allele or haplotype string to IMGT format.
    Examples:
        'A_0101' => 'A*01:01'
        'DQA1_0101__DQB1_0201' => 'DQA1*01:01~DQB1*02:01'
    """
    # Split on haplotype delimiter
    parts = allele_str.split('__')
    
    imgt_parts = []
    for part in parts:
        # Match prefix and allele number (allowing optional suffixes like 'N')
        match = re.match(r"([A-Z0-9]+)_([0-9]{4,5}[A-Z]*)", part)
        if match:
            gene, digits = match.groups()
            # Insert colon between fields (2-digit or 3-digit groups)
            if len(digits) >= 4:
                imgt = f"{gene}*{digits[:2]}:{digits[2:]}"
            else:
                imgt = f"{gene}*{digits}"
            imgt_parts.append(imgt)
        else:
            # Fallback: return original if no match
            imgt_parts.append(part)
    
    # Join haplotype parts with tilde (~)
    return "/".join(imgt_parts)

# Special Case
def get_train_result():
    training_file = '/fh/fast/gilbert_p/kmayerbl/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v3/sample_hla_x_train.csv'
    test_file     = '/fh/fast/gilbert_p/kmayerbl/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v3/sample_hla_x_test.csv'
    a = pd.read_csv(training_file, index_col = 0)
    b = pd.read_csv(test_file,     index_col = 0) 
    return pd.concat([a,b])

def get_musvosvi():
    file= '/fh/fast/gilbert_p/kmayerbl/tcrtest/model/XSTUDY_ALL_FEATURE_L1_v3/sample_hla_x_musvosvi.csv'
    return pd.read_csv(file,     index_col = 0) 

prev = dict()
for x in ['TOWB','TOW','TOWA', 'VALA','VALB', "MIRA",'ROS','ROSA','ROSB','TRAIN','MUSVOSVI']:
    if x == "TRAIN":
        H = get_train_result()
    elif x == "MUSVOSVI":
        H = get_musvosvi()
    else:
        hla_file = f'/fh/fast/gilbert_p/kmayerbl/TCR2HLA_data/raw_data/{x}_hla.tsv' 
        H = pd.read_csv(hla_file, sep = "\t", index_col = 0)
    prev[x] = H.mean()



from tcrtest.classify import map_allele2
# Get training data 
df = pd.DataFrame(prev)
df['binary_imgt'] = pd.Series(df.index).apply(lambda x: to_imgt_format(x)).to_list()
df['binary']      = df.index
qf = '/fh/fast/gilbert_p/kmayerbl/TCR2HLA/tcrtest/models/XSTUDY_ALL_FEATURE_L1_v4e/XSTUDY_ALL_FEATURE_L1_v4e.query.csv'
qdf = pd.read_csv(qf, index_col = 0)

df = df.loc[sorted(qdf.binary.unique())]
df['locus'] = df['binary'].apply(lambda x: map_allele2(x))
df[['locus','binary','binary_imgt','TOWB','TOW','TOWA', 'VALA','VALB', 
    "MIRA",'ROS','ROSA','ROSB','TRAIN','MUSVOSVI']].\
    to_csv('supporting_information/TCR2HLA_dataset_allele_prevalence.csv', index = False)