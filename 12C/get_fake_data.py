import pandas as pd
import numpy as np

df = pd.read_csv('Data/Fe56.csv')
columns = ['Z', 'A', 'E0', 'ThetaDeg', 'nu', 'cross', 'error', 'dataSet']
new_rows = []
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[1], x[0]))

for E0, ThetaDeg, dataSet in value_pairs:
    sub = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
    nu_min = sub['nu'].min()
    nu_max = sub['nu'].max()
    nu_vals = np.arange(nu_min, nu_max + 1e-6, 0.002)
    for nu in nu_vals:
        new_rows.append([26, 56, E0, ThetaDeg, round(nu, 5), 1, 0, dataSet])
new_df = pd.DataFrame(new_rows, columns=columns)
new_df.to_csv('Data/Fe56_Fake_raw.csv', index=False)
