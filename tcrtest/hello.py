import os
from tcrtest.paths import path_to_models

def main():
    print(f"TCR2HLA contains a number of precomputed models and calibrations in the folder:\n\t{path_to_models}")
    contents = os.listdir(path_to_models)  # Fixed assignment operator
    model_subfolders = list()
    for subfolder in contents:
        if not subfolder.startswith("."):
            model_subfolders.append(os.path.join(path_to_models, subfolder))
            print(f"\t{subfolder}")
            model_contents = os.listdir(os.path.join(path_to_models, subfolder))
            for item in model_contents:
                print(f"\t\t{item}")
    print("Completed Successfully")
    return model_subfolders

if __name__ == "__main__":
    x = main()
