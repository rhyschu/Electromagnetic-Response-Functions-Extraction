import pandas as pd
import numpy as np

Q2centers = [0.010, 0.020, 0.026, 0.040, 0.056, 0.093, 0.120, 0.160, 0.265, 0.380, 0.500, 0.800, 1.250, 1.750, 2.250, 2.750, 3.250, 3.750]
Q2bins = [0.004, 0.015, 0.025, 0.035, 0.045, 0.070, 0.100, 0.145, 0.206, 0.322, 0.438, 0.650, 1.050, 1.500, 2.000, 2.500, 3.000, 3.500, 4.000]
Q2bin_names = ['[0.004,0.015]', '[0.015,0.025]', '[0.025,0.035]', '[0.035,0.045]', '[0.045,0.070]', '[0.070,0.100]', '[0.100,0.145]', '[0.145,0.206]', '[0.206,0.322]',
                '[0.322,0.438]', '[0.438,0.650]', '[0.650,1.050]', '[1.050,1.500]', '[1.500,2.000]', '[2.000,2.500]', '[2.500,3.000]', '[3.000,3.500]', '[3.500,4.000]']

small_Q2 = False
df = pd.read_csv('Data/Fe56.csv')
columns = ['Z', 'A', 'E0', 'ThetaDeg', 'nu', 'cross', 'error', 'dataSet']
new_rows = []
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[1], x[0]))

for E0, ThetaDeg, dataSet in value_pairs:
    sub = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
    nu_min = sub['nu'].min()
    nu_max = sub['nu'].max()
    nu_vals = np.arange(nu_min, nu_max + 1e-6, 0.005)
    for nu in nu_vals:
        new_rows.append([26, 56, E0, ThetaDeg, round(nu, 5), 1, 0, dataSet])
df = pd.DataFrame(new_rows, columns=columns)

if small_Q2:
    df["Veff"] = 0.0089
    df["Eeff"] = df["E0"] + df["Veff"]
    df["Ep"] = df["E0"] - df["nu"]
    df["Ep_eff"] = df["Ep"] + df["Veff"]
    df["ThetaRad"] = df["ThetaDeg"]*np.pi/180
    df["sin2(T/2)"] = (np.sin(df["ThetaRad"] / 2))**2
    df["Q2eff"] = 4 * df["Eeff"] * df["Ep_eff"] * df["sin2(T/2)"]
    df['Q2bin'] = 0
    df['Q2center'] = 0
    df["Q2bin"] = pd.cut(x=df["Q2eff"], bins=Q2bins, labels=Q2bin_names, right=True)
    df["Q2center"] = pd.cut(x=df["Q2eff"], bins=Q2bins, labels=Q2centers, right=True)
    df['Q2center'] = pd.to_numeric(df['Q2center'])
    Q2low = []
    Q2high = []
    for i, Q2 in enumerate(Q2centers):
        if i == 0:
            delta_low = 0.1 * (Q2centers[i+1] - Q2)
        else:
            delta_low = 0.1 * (Q2 - Q2centers[i-1])
        if i == len(Q2centers) - 1:
            delta_high = 0.1 * (Q2 - Q2centers[i-1])
        else:
            delta_high = 0.1 * (Q2centers[i+1] - Q2)
        Q2low.append(Q2 - delta_low)
        Q2high.append(Q2 + delta_high)
    mask = np.zeros(len(df), dtype=bool)
    for i in range(len(Q2centers)):
        q2c = Q2centers[i]
        low = Q2low[i]
        high = Q2high[i]
        mask |= (df["Q2center"] == q2c) & (df["Q2eff"] >= low) & (df["Q2eff"] <= high)
    df = df[mask].reset_index(drop=True)

df = df[columns]
df.to_csv('Data/Fe56_Fake_raw.csv', index=False)
