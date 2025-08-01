import numpy as np
import pandas as pd
import os

mass_nucleon = 0.938273
degree = 8
qv_values = [0.1, 0.148, 0.167, 0.205, 0.24, 0.3, 0.38, 0.475, 0.57, 0.649, 0.756, 0.991, 1.619, 1.921, 2.213, 2.5, 2.783, 3.5]  
q2_values = [0.01, 0.02, 0.026, 0.04, 0.056, 0.093, 0.12, 0.16, 0.265, 0.38, 0.5, 0.8, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75]
df_data_full = pd.read_csv('df_Fe56_thisAnalysis.csv',index_col = False)
df_Photo = pd.read_csv('df_Fe56_Photoproduction_qv.csv',index_col = False)
os.makedirs("Qvedges_new", exist_ok=True)
os.makedirs("Q2edges_new", exist_ok=True)

def find_closest(array, value):
    return array[np.argmin(np.abs(array - value))]

for qvcenter in qv_values[3:17]:
    df_original = pd.read_csv('Qvedges/'+f'Qvedge_{qvcenter}.csv',index_col = False)
    df_data = df_data_full[df_data_full['qvcenter'] == qvcenter]
    df_original['RTTOT'] -= df_original['RTNS']
    df_original['RLTOT'] -= df_original['RLNS']
    df_original['RTTOT'] *= 1e3
    df_original['RLTOT'] *= 1e3
    df_original["W2"] = mass_nucleon**2 + 2 * mass_nucleon * df_original["nu"] - df_original["q2"]
    W2_data = df_data['W2'].values
    RL_data = df_data['RL'].values
    RT_data = df_data['RT'].values
    W2_original = df_original['W2'].values
    RL_original = df_original['RLTOT'].values
    RT_original = df_original['RTTOT'].values
    RL_diff = []
    RT_diff = []
    for i, W2 in enumerate(W2_data):
        idx = np.argmin(np.abs(W2_original - W2))
        RL_diff.append(RL_data[i] - RL_original[idx])
        RT_diff.append(RT_data[i] - RT_original[idx])
    RL_diff_fit_coeffs = np.polyfit(W2_data, RL_diff, deg=degree)
    RT_diff_fit_coeffs = np.polyfit(W2_data, RT_diff, deg=degree)
    RL_diff_fit_values = np.polyval(RL_diff_fit_coeffs, df_original["W2"].values)
    RT_diff_fit_values = np.polyval(RT_diff_fit_coeffs, df_original["W2"].values)
    df_original["RLTOT"] += RL_diff_fit_values
    df_original["RTTOT"] += RT_diff_fit_values
    df_new = df_original[["qv", "q2", "ex", "nu", "W2", "RLTOT", "RTTOT"]]
    row_match = df_Photo[df_Photo["qvcenter"] == qvcenter]
    RT_Photon = row_match["RT"].values[0]
    new_row = {
        "qv": qvcenter,
        "q2": 0.0,
        "ex": qvcenter,
        "nu": qvcenter,
        "W2": mass_nucleon**2 + 2 * mass_nucleon * qvcenter,
        "RLTOT": np.nan,
        "RTTOT": row_match["RT"].values[0] * 1e3
    }
    df_new = pd.concat([df_new, pd.DataFrame([new_row])], ignore_index=True)
    df_new.to_csv('Qvedges_new/'+f'Qvedge_{qvcenter}.csv', index=False)

for Q2center in q2_values[2:17]:
    df_original = pd.read_csv('Q2edges/'+f'Q2edge_{Q2center}.csv',index_col = False)
    df_data = df_data_full[df_data_full['Q2center'] == Q2center]
    df_original['RTTOT'] -= df_original['RTNS']
    df_original['RLTOT'] -= df_original['RLNS']
    df_original['RTTOT'] *= 1e3
    df_original['RLTOT'] *= 1e3
    df_original["W2"] = mass_nucleon**2 + 2 * mass_nucleon * df_original["nu"] - df_original["q2"]
    W2_data = df_data['W2'].values
    RL_data = df_data['RL'].values
    RT_data = df_data['RT'].values
    W2_original = df_original['W2'].values
    RL_original = df_original['RLTOT'].values
    RT_original = df_original['RTTOT'].values
    RL_diff = []
    RT_diff = []
    for i, W2 in enumerate(W2_data):
        idx = np.argmin(np.abs(W2_original - W2))
        RL_diff.append(RL_data[i] - RL_original[idx])
        RT_diff.append(RT_data[i] - RT_original[idx])
    RL_diff_fit_coeffs = np.polyfit(W2_data, RL_diff, deg=degree)
    RT_diff_fit_coeffs = np.polyfit(W2_data, RT_diff, deg=degree)
    RL_diff_fit_values = np.polyval(RL_diff_fit_coeffs, df_original["W2"].values)
    RT_diff_fit_values = np.polyval(RT_diff_fit_coeffs, df_original["W2"].values)
    df_original["RLTOT"] += RL_diff_fit_values
    df_original["RTTOT"] += RT_diff_fit_values
    df_new = df_original[["qv", "q2", "ex", "nu", "W2", "RLTOT", "RTTOT"]]
    df_new.to_csv('Q2edges_new/'+f'Q2edge_{Q2center}.csv', index=False)
