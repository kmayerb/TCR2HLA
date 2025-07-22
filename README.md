<img width="2500" height="834" alt="Image" src="https://github.com/user-attachments/assets/fea9110e-97c1-4886-ab04-19c2bda0c67a" />

**TCR2HLA** is a tool for predicting common HLA genotypes from T cell receptor (TCR) sequencing data using pretrained machine learning models.

## Table of Contents

- [Getting Started: Inferring HLA genotypes from TCR data](#getting-started)
  - [Download Example Data](#download-example-data)
  - [Run Inference Interactively (Python)](#run-inference-interactively-python)
  - [Run Inference from the Command Line](#run-inference-from-the-command-line)
  - [Parameters for TCR2HLA](#parameters-for-tcr2hla)
  - [Outputs](#outputs)
  - [TCR2HLA Output Files](#tcr2hla-output-files)
- [Using Inferred HLA genotypes to Find Cohort-Specific HLA-associated TCRs](#use-inferred-hla-genotypes-to-find-cohort-specific-hla-associated-tcrs)
  - [Step 1: Create a .zip file of all your repertoires](#step-1-create-a-zip-file-of-all-your-repertoires)
  - [Step 2: Define a parsing function to yield useful columns from each repertoire](#step-2-define-a-parsing-function-to-yield-useful-columns-from-each-repertoire)
  - [Step 3: Find new HLA associated TCRs using inferred HLA genotypes of each donor](#step-3-find-new-hla-associated-tcrs-using-inferred-hla-genotypes-of-each-donor)
  - [Step 4: Assemble HLA-associated TCRs across V partitions](#step-4-assemble-hla-associated-tcrs-across-v-partitions)
- [Installation](#installation)
- [Dependencies](#dependencies)

## Getting Started

Below are simple examples to get you up and running.

### Download Example Data

Download datasets in minimal format for validation:

> 💡 **Tip**: Validation data downloaded with <download_minimal_validation_data> is formated as concisely as possible with only 2 columns: vfamcdr3 (e.g. V09CASRIRRSENGTF) and productive frequency of each clonotype. `TCR2HLA_data.zip` includes separate folders by dataset. For full repertoires used for external validation of TCR2HLA, see the original publications: Rawat et al. 2024, Towlerton et al. 2022, and Rosati et al. 2022 and Nolan et al. 2025 (citations below).

```python
from tcrtest.examples import download_minimal_validation_data

download_minimal_validation_data(
    unzip=True,
    dest_dir='TCR2HLA_data',
    filename="TCR2HLA_data.zip"
)
```

### Run Inference Interactively (Python)

Use the TCR2HLA function to run inference from Python:

```python
from tcrtest.infer import main as TCR2HLA
cpus = 24
TCR2HLA([
    "--input_folder", "TCR2HLA_data/TOWA_minimal",
    "--model_name", "XSTUDY_ALL_FEATURE_L1_v4e",
    "--calibration_name", "XSTUDY_ALL_FEATURE_L1_v4e_HS2",
    "--name", "TOWA_v4e_HS2",
    "--project_folder", "demo/TOWA_v4e_HS2",
    "--cpus", f"{cpus}",
    "--sep", "\t",
    "--truth_values", "TCR2HLA_data/TOWA_hla.tsv",
    "--gate1", "0.1", 
    "--gate2", "0.9",
    "--force"
])
```

### Run Inference from the Command Line


You can also run the same pipeline from the terminal:


> 💡 **Tip**: if you pip install TCR2HLA you can call a commandline program directly with TCR2HLA. Otherwise you can invoke it from the repository's root directory with `python tcrtest/infer.py`. 

```bash
TCR2HLA \ 
    --input_folder TCR2HLA_data/TOWA_minimal \
    --model_name XSTUDY_ALL_FEATURE_L1_v4e \
    --calibration_name XSTUDY_ALL_FEATURE_L1_v4e_HS2 \
    --name TOWA_v4e_HS2 \
    --project_folder demo/TOWA_v4e_HS2 \
    --cpus 24 \
    --sep "\t" \
    --truth_values 'TCR2HLA_data/TOWA_hla.tsv' \
    --gate1 0.1 \
    --gate2 0.9 \
    --force 
```

### Parameters for `TCR2HLA`

| Argument                   | Type    | Default                          | Description |
|---------------------------|---------|----------------------------------|-------------|
| `--zipfile`               | str     | –                                | Path to a zip archive containing raw sequencing data. Required if `--input_folder` is not used. |
| `--input_folder`          | str     | –                                | Path to a directory with preprocessed files (CSV/TSV). Required if `--zipfile` is not used. |
| `--truth_values`          | str     | None                             | Path to a TSV/CSV file containing ground truth HLA types (optional, for evaluation). |
| `--force`                 | flag    | False                            | Force re-computation of the occurrence matrix, even if one already exists. You could omit this if you want to repeat with new gate1/gate2 values |
| `--cpus`                  | int     | 2                                | Number of CPU cores to use for parallel processing. |
| `--name`                  | str     | –                                | A short identifier for the run; used in naming output files. |
| `--project_folder`        | str     | `"demo_project"`                 | Directory where intermediate and final outputs will be stored. |
| `--get_col`               | str     | `"productive_frequency"`         | Feature column to extract during matrix building. |
| `--on`                    | str     | `"vfamcdr3"`                     | Feature type to compute occurrence matrix on. |
| `--model_name`            | str     | `"XSTUDY_ALL_FEATURE_L1_v4e"`    | Name of the pretrained model to use. |
| `--calibration_name`      | str     | `"XSTUDY_ALL_FEATURE_L1_v4e_HS2"`| Calibration model used to adjust raw model outputs. |
| `--test_mode`             | flag    | False                            | Run on a small subset of files for fast debugging/testing. |
| `--min_value`             | float   | `2e-6`                           | Minimum frequency threshold for inclusion in occurrence matrix. |
| `--download_towlerton_zip`| flag    | False                            | If set, downloads a demonstration dataset (Towlerton). |
| `--parse_adaptive_files`  | flag    | False                            | Parse Adaptive Biotech files (v2 format) automatically if using zipfile as input |
| `--sep`                   | str     | `","`                            | Field delimiter in input files. Use `"\\t"` for tab-separated files. |
| `--gate1`                 | float   | 0.5                              | Lower decision threshold used for binary prediction (gate 1). |
| `--gate2`                 | float   | 0.5                              | Upper decision threshold used for binary prediction (gate 2). |



### Outputs

```
demo/TOWA_v4e_HS2/
├── query_x_TOWA_v4e_HS2.npz
├── query_x_TOWA_v4e_HS2.npz.columns.csv
├── samples_TOWA_v4e_HS2_x_calibrated_probs.tsv
├── samples_TOWA_v4e_HS2_x_calibrated_probs_boolean.tsv
├── samples_TOWA_v4e_HS2_x_calibrated_probs_gated.tsv
├── samples_TOWA_v4e_HS2_x_calibrated_probs_gated_boolean.tsv
├── samples_TOWA_v4e_HS2_x_decision_scores.csv
├── samples_x_TOWA_v4e_HS2.csv
├── sample_x_TOWA_v4e_HS2_performance.csv
├── sample_x_TOWA_v4e_HS2_predictions.long.csv
├── sample_x_TOWA_v4e_HS2_predictions.observations.long.csv
```


## TCR2HLA Output Files

After running TCR2HLA, the following files will be generated in the specified `--project_folder` (e.g., `demo/TOWA_v4e_HS2/`):

| File Name | Description |
|-----------|-------------|
| `query_x_TOWA_v4e_HS2.npz` | Compressed NumPy array of the sparse feature matrix used for prediction. Each row corresponds to a feature (e.g., TCR exact or inexact match), and each column corresponds to a sample. |
| `query_x_TOWA_v4e_HS2.npz.columns.csv` | List of sample IDs (column headers) associated with the matrix stored in the `.npz` file. |
| `samples_x_TOWA_v4e_HS2.csv` | Sample-level metadata used during inference, including computed covariates such as log10 clone counts. |
| `samples_TOWA_v4e_HS2_x_calibrated_probs.csv` | Calibrated probability scores (between 0 and 1) for each HLA allele prediction, per sample. These are adjusted using the specified calibration model. |
| `samples_TOWA_v4e_HS2_x_calibrated_probs_gated.csv` | Calibrated probability scores (between 0 and 1) for each HLA allele prediction, per sample. These are adjusted using the specified calibration model and values between gate1 and gate2 are masked out as NAs. |
| `samples_TOWA_v4e_HS2_x_decision_scores.csv` | Raw decision scores (uncalibrated model outputs) from the classifier before calibration. |
| `sample_x_TOWA_v4e_HS2_predictions.long.csv` | Long-format file containing binary HLA predictions (`True` or `False`) along with calibrated probabilities for each sample-allele pair. |
| `sample_x_TOWA_v4e_HS2_predictions.observations.long.csv` | Similar to the predictions file above, but also includes observed (ground truth) labels if provided via `--truth_values`. Useful for evaluation. |
| `sample_x_TOWA_v4e_HS2_performance.csv` | Evaluation metrics (e.g., balanced accuracy, AUC, sensitivity, specificity) computed against ground truth labels using calibrated probabilities and user-defined thresholds (`--gate1`, `--gate2`). |
| `samples_TOWA_v4e_HS2_x_calibrated_probs_gated_boolean.tsv` | Boolean version of the gated calibrated probabilities: values > 0.5 and >gate2 are set to `True`, NAs are retained. Useful for downstream binary analyses. |

### Use Inferred HLA genotypes to Find Cohort-Specific HLA-associated TCRs

#### Step 1 create a .zip file of all your repertoires
```bash
cd TCR2HLA_data/TOWA_minimal
zip ../TOWA_minimal.zip *.tsv
```

#### Step 2: define a parsing function to yield useful columns from each repertoire
```python
def parse_minimal(df,f, min_value = 2E-6, out_cols = ['v','vfamcdr3','amino_acid','productive_frequency','sample_id','v_gene','j_gene']):
    df['v'] = df['vfamcdr3'].str[0:3]
    df['amino_acid'] = df['vfamcdr3'].str[3:]
    if min_value is not None:
        df = df[df['productive_frequency'] > min_value ].reset_index(drop = True)  
    df = df[ df['v'] != 'V0A'].reset_index(drop = True)  
    df['sample_id'] = os.path.basename(f).replace(".tsv","").replace('.csv','')
    df['v_gene'] = 'v_gene' # we put placehoders here since minimal files don't include full v-gene information 
    df['j_gene'] = 'j_gene'
    dfout = df[ out_cols].sort_values('productive_frequency', ascending = False).reset_index(drop = True)  
    return(dfout)
```


#### Step 3: find new HLA associated TCRs using inferred HLA genotypes of each donor

```python 
from tcrtest.ui import VfamCDR3, get_revelant_binaries
import os
import pandas as pd
cpus = 24
project_folder = 'demo/TOWA_v4e_HS2/'
v = VfamCDR3(
    project_folder   = project_folder,
    input_zfile      = 'TCR2HLA_data/TOWA_minimal.zip',
    cpus = cpus)
v.list_raw_files()
v.stratv_v2_parmap(parse_func = parse_minimal)
v.combine_stratv()
v.get_combined_vfam_filelist()
subject_binary_file1 = 'demo/TOWA_v4e_HS2/samples_TOWA_v4e_HS2_x_calibrated_probs_gated_boolean.tsv'
bin_vars = get_revelant_binaries(subject_binary_file1, .1)
# Note: this may require that you make slight modifications the tcrtest/run.py file to match your Python and SLURM environment.
v.run_direct_finder(
    subject_binary_file = subject_binary_file1,
    binary_variables = bin_vars ,
    pattern='_combined.csv',
    query_cdr3_col='amino_acid',
    query_v_col='v_gene',
    query_j_col='j_gene',
    query_vfam_col='v',
    sample_id_col='sample_id',
    min_occur=1,
    min_collisions=3,
    min_pub=3,
    max_pval=0.001,
    max_pval_override0=0.001,
    max_pval_override1=0.001,
    allow_missing=True, # critical to set to True if NaN in HLA matrix
    partition='short',
    force=True,
    launch=False, # set to True if you want to launch sbatch jobs
    setup_commands = 'source ~/.bashrc && conda activate tcrdist311' # specific to your environment
)
v.with_slurm('run_direct_finder') # Launches all jobs with slurm 
v.with_single_machine('run_direct_finder') # if you want to run one V partition at a time on current macine
```

> 💡 **Tip**: You can customize your SLURM job by passing to <setup_commands> a string like `module load python/3.11`.


#### Step 4: assemble HLA-associated TCRs across V partitions
```python
# When all jobs completed -- get assembed HLA-associated exact and inexact TCR features
df0, df1, df0hq, df1hq = v.assemble_association_files(endswith = "binvar.csv")
```

## Setup 

### Installation
You can clone or install TCR2HLA in two ways:

**1. Clone the repository and install locally:**

```bash
git clone https://github.com/kmayerb/TCR2HLA.git
cd TCR2HLA
```

**2. Install directly from GitHub using pip:**

```bash
pip install "git+https://github.com/kmayerb/TCR2HLA.git"
```

### Dependencies

TCR2HLA requires the following Python packages:

- `numpy`
- `pandas`
- `scipy`
- `psutil`
- `tqdm`
- `progress`
- `parmap`
- `scikit-learn`

These dependencies will be installed automatically when you install TCR2HLA using `pip install .` or `pip install "git+https://github.com/kmayerb/TCR2HLA.git"`. 

#### Validation Data Sources

* Rawat, P. et al. Identification of a type 1 diabetes-associated T cell receptor repertoire signature from the human peripheral blood. medRxiv 2024.12.10.24318751 (2024).

* Towlerton, A. M. H., Ravishankar, S., Coffey, D. G., Puronen, C. E. & Warren, E. H. Serial analysis of the T-cell receptor β-chain repertoire in people living with HIV reveals incomplete recovery after long-term antiretroviral therapy. Front. Immunol. 13, 879190 (2022).

* Rosati, E. et al. A novel unconventional T cell population enriched in Crohn’s disease. Gut 71, 2194–2204 (2022).

* Nolan, S. et al. A large-scale database of T-cell receptor beta sequences and binding associations from natural and synthetic exposure to SARS-CoV-2. Front. Immunol. 16, 1488851 (2025).

## License

The code in this repository is licensed under the [MIT License](LICENSE).

The model weights, calibration weights, and features in this repository are licensed under the 
[Creative Commons Attribution-BY 4.0 International License](https://creativecommons.org/licenses/by/4.0/).

© 2025 Koshlan Mayer-Blackwell.