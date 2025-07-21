# First Run Standard Model

import os 
import sys
import pandas as pd
cpus = 24
completed = dict()

for x in ['TOWB','TOW','TOWA', 'VALA','VALB', "MIRA",'ROSA','ROSB']:
    for calibration in ["_S", "_HS2"]:
        if calibration == "_S":
            cal_tag = ""
        else:
            cal_tag = calibration
        print(f"Running validation for {x} with calibration:{calibration}")
        cmd = f"""python tcrtest/infer.py \\
--input_folder TCR2HLA_data/{x}_minimal \\
--model_name XSTUDY_ALL_FEATURE_L1_v4e \\
--calibration_name XSTUDY_ALL_FEATURE_L1_v4e{cal_tag} \\
--name {x}_v4e{calibration} \\
--project_folder supporting_information/{x}_v4e{calibration} \\
--cpus {cpus} \\
--sep "\t" \\
--truth_values '/fh/fast/gilbert_p/kmayerbl/TCR2HLA_data/raw_data/{x}_hla.tsv' \\
--force 
"""
        print(cmd)
        os.system(cmd)
        completed[f"{x}_v4e{calibration}"] = cmd

for x in [ 'TOWB',"TOW",'TOWA','VALA','VALB', "MIRA",'ROSA','ROSB']:
    for calibration in ["_S", "_HS2"]:
        if calibration == "_S":
            cal_tag = ""
        else:
            cal_tag = calibration
        print(f"Running validation for {x} with calibration:{calibration}")
        cmd = f"""python tcrtest/infer.py \\
--input_folder TCR2HLA_data/{x}_minimal \\
--model_name XSTUDY_ALL_FEATURE_L1_v4e \\
--calibration_name XSTUDY_ALL_FEATURE_L1_v4e{cal_tag} \\
--name {x}_v4e{calibration}_gated \\
--project_folder supporting_information/{x}_v4e{calibration}_gated \\
--cpus {cpus} \\
--sep "\t" \\
--truth_values '/fh/fast/gilbert_p/kmayerbl/TCR2HLA_data/raw_data/{x}_hla.tsv' \\
--force \\
--gate1 0.1 \\
--gate2 0.9 
"""
        print(cmd)
        os.system(cmd)
        completed[f"{x}_v4e{calibration}_gated"] = cmd
# SAVE ALL OF THE COMMANDS TO A SINGLE FILE
with open("supporting_information/TCR2HLA_validation_bash_commands.sh", 'w') as fh:
    for k,v in completed.items():
        fh.write(f"#{k}\n")
        fh.write(f"{v}\n")

import os 
import pandas as pd
store = list()
for x in ['VALA', 'VALB','TOWA','TOWB', 'TOW', "MIRA",'ROSA', 'ROSB' ]:
    for calibration in ["_S","_HS2"]:
      tag= f"{x}_v4e{calibration}"
      d1 = pd.read_csv(f"supporting_information/{tag}/sample_x_{tag}_performance.csv", sep = ",")
      d2 = pd.read_csv(f"supporting_information/{tag}_gated/sample_x_{tag}_gated_performance.csv", sep = ",")
      store.append(d1)
      store.append(d2)    
d = pd.concat(store).reset_index()
d[['dataset','model','calibration','gated']]=d['name'].str.split("_",expand = True)
d.to_csv("supporting_information/TCR2HLA_validation_runs.csv", index = False)

