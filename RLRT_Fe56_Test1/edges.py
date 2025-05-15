import subprocess
import pandas as pd

qv_values = [0.1, 0.148, 0.167, 0.205, 0.24, 0.3, 0.38, 0.475, 0.57, 0.649, 0.756, 0.991, 1.619, 1.921, 2.213, 2.5, 2.783, 3.5]  
q2_values = [0.01, 0.02, 0.026, 0.04, 0.056, 0.093, 0.12, 0.16, 0.265, 0.38, 0.5, 0.8, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75]
header = ["qv", "q2", "ex", "nu", "RTTOT", "RLTOT", "RTQE", "RLQE", "RTIE", "RLIE", "RTE", "RLE", "RTNS", "RLNS"]
QE_shift = 0.0

def shift(df, QE_shift):
    nu_to_RTQE = dict(zip(df["nu"].round(5), df["RTQE"]))
    nu_to_RLQE = dict(zip(df["nu"].round(5), df["RLQE"]))
    nu_to_RTNS = dict(zip(df["nu"].round(5), df["RTNS"]))
    nu_to_RLNS = dict(zip(df["nu"].round(5), df["RLNS"]))
    updated_rows = []  
    for _, row in df.iterrows():
        updated_nu = (row["nu"] - QE_shift).round(5)
        if updated_nu in nu_to_RTQE:
            RTQE_prime = nu_to_RTQE[updated_nu]
            RLQE_prime = nu_to_RLQE[updated_nu]
            RTNS_prime = nu_to_RTNS[updated_nu]
            RLNS_prime = nu_to_RLNS[updated_nu]
            row["RTTOT"] = row["RTTOT"] - row["RTQE"] + RTQE_prime 
            row["RTQE"] = RTQE_prime
            row["RTNS"] = RTNS_prime
            row["RLTOT"] = row["RLTOT"] - row["RLQE"] + RLQE_prime
            row["RLQE"] = RLQE_prime
            row["RLNS"] = RLNS_prime
            updated_rows.append(row)
    updated_df = pd.DataFrame(updated_rows)
    return updated_df

for q in qv_values:
    process = subprocess.run(["./responseqv"], input=f"{q}\n", text=True, capture_output=True)
    with open("responseqv_output.txt", "r") as f:
        lines = f.readlines()
    data = [line.strip().split() for line in lines if line.strip()]
    df = pd.DataFrame(data, columns=header)
    df = df.astype(float)
    df_final = shift(df, QE_shift)
    csv_filename = f"Qvedges/Qvedge_{q}.csv"
    df_final.to_csv(csv_filename, index=False)

for q in q2_values:
    process = subprocess.run(["./responseq2"], input=f"{q}\n", text=True, capture_output=True)
    with open("responseq2_output.txt", "r") as f:
        lines = f.readlines()
    data = [line.strip().split() for line in lines if line.strip()]
    df = pd.DataFrame(data, columns=header)
    df = df.astype(float)
    df_final = shift(df, QE_shift)
    csv_filename = f"Q2edges/Q2edge_{q}.csv"
    df_final.to_csv(csv_filename, index=False)
