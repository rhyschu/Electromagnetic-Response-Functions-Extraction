import numpy as np
import pandas as pd

df = pd.read_csv('Fe56_Fake_NoNuc_raw.csv',index_col = False)
df['Z'] = 26
df['A'] = 56
df['dataSet'] = 'FakeQ2001'
df["ThetaRad"] = df["ThetaDeg"] * np.pi / 180
df["sin2(T/2)"] = np.sin(df["ThetaRad"] / 2) ** 2
df["E0"] = (df["nu"] + np.sqrt(df["nu"]**2 + df["q2"] / df["sin2(T/2)"])) / 2
df['cross'] = 1
df['error'] = 0
df.to_csv('Fe56_Fake_NoNuc_Q2001.csv',index=False)
