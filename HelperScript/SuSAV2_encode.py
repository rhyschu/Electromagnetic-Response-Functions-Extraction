import os
import pandas as pd

qvcenters = [0.100, 0.148, 0.167, 0.205, 0.240, 0.300, 0.380, 0.475, 0.570, 0.649, 0.756, 0.991, 1.619, 1.921, 2.213, 2.500, 2.783, 3.500]
Q2centers = [0.010, 0.020, 0.026, 0.040, 0.056, 0.093, 0.120, 0.160, 0.265, 0.380, 0.500, 0.800, 1.250, 1.750, 2.250, 2.750, 3.250, 3.750]
data_dir = "SuSAV2_fit_New"

def extract_center_and_index_qv(filename):
    num = filename.split('_')[3] 
    val = float(f"{num[0]}.{num[1:]}")
    val = round(val, 3)
    index = qvcenters.index(val)
    return val, index

def extract_center_and_index_Q2(filename):
    num = filename.split('_')[3]  
    val = float(f"{num[0]}.{num[1:]}")
    val = round(val, 3)
    index = Q2centers.index(val)
    return val, index

all_data = []

for fname in os.listdir(data_dir):
    full_path = os.path.join(data_dir, fname)
    if fname.startswith("56Fe_BC_q"):
        try:
            bin_val, bin_index = extract_center_and_index_qv(fname)
            bin_type = "qv"
        except Exception as e:
            print(f"Skipping {fname} (qv): {e}")
            continue
    elif fname.startswith("56Fe_BC_Q2"):
        try:
            bin_val, bin_index = extract_center_and_index_Q2(fname)
            bin_type = "Q2"
        except Exception as e:
            print(f"Skipping {fname} (Q2): {e}")
            continue
    else:
        continue
    try:
        df = pd.read_csv(full_path, sep='\s+', header=None)
        df.insert(0, "bin_type", bin_type)
        df.insert(1, "bin_index", bin_index)
        df.insert(2, "center", bin_val)
        all_data.append(df)
    except Exception as e:
        print(f"Error reading {fname}: {e}")

if all_data:
    result = pd.concat(all_data, ignore_index=True)
    result.columns = ['bin_type', 'bin_index', 'center', 'nu', 'q/Q2', 'RLQE', 'RTQE', 'RLMEC', 'RTMEC', 'RLIE', 'RTIE']
    result.to_csv('Fe56_SuSAV2_Fit.csv', index=False)
else:
    print("No valid data files processed.")
