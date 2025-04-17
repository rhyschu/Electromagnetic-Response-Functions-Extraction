import subprocess
import pandas as pd

qv_values = [0.1, 0.148, 0.167, 0.205, 0.24, 0.3, 0.38, 0.475, 0.57, 0.649, 0.756, 0.991, 1.619, 1.921, 2.213, 2.5, 2.783, 3.5]  
q2_values = [0.01, 0.02, 0.026, 0.04, 0.056, 0.093, 0.12, 0.16, 0.265, 0.38, 0.5, 0.8, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75]
header = ["qv", "q2", "ex", "nu", "RTTOT", "RLTOT", "RTQE", "RLQE", "RTIE", "RLIE", "RTE", "RLE", "RTNS", "RLNS"]

for q in qv_values:
    process = subprocess.run(["./responseqv"], input=f"{q}\n", text=True, capture_output=True)
    with open("responseqv_output.txt", "r") as f:
        lines = f.readlines()
    data = [line.strip().split() for line in lines if line.strip()]
    df = pd.DataFrame(data, columns=header)
    df = df.astype(float)
    csv_filename = f"Qvedges/Qvedge_{q}.csv"
    df.to_csv(csv_filename, index=False)

for q in q2_values:
    process = subprocess.run(["./responseq2"], input=f"{q}\n", text=True, capture_output=True)
    with open("responseq2_output.txt", "r") as f:
        lines = f.readlines()
    data = [line.strip().split() for line in lines if line.strip()]
    df = pd.DataFrame(data, columns=header)
    df = df.astype(float)
    csv_filename = f"Q2edges/Q2edge_{q}.csv"
    df.to_csv(csv_filename, index=False)
