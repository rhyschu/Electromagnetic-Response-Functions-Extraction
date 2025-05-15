import pandas as pd
from scipy.interpolate import interp1d

data_left = 'Data/Fe56_Fit_Left6MeV.csv'
data_right = 'Data/Fe56_Fit_Right13MeV.csv'
data_result = 'Data/Fe56_Fit_LeftRightCombined.csv'
W2_split = 0.95**2
df_left = pd.read_csv(data_left)
df_right = pd.read_csv(data_right)
df_left = df_left[df_left['W2'] < W2_split]
df_right = df_right[df_right['W2'] >= W2_split]
pairs1 = set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df_left.iterrows())
pairs2 = set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df_right.iterrows())
value_pairs = sorted(pairs1.union(pairs2), key=lambda x: (x[2], x[1], x[0]))
combined_data = []

for value_pair in value_pairs:
    E0, ThetaDeg, dataSet = value_pair
    filtered_data_left = df_left[(df_left['E0'] == E0) & (df_left['ThetaDeg'] == ThetaDeg) & (df_left['dataSet'] == dataSet)].copy()
    filtered_data_right = df_right[(df_right['E0'] == E0) & (df_right['ThetaDeg'] == ThetaDeg) & (df_right['dataSet'] == dataSet)].copy()
    if filtered_data_left.empty or filtered_data_right.empty:
        combined_data.append(filtered_data_left)
        combined_data.append(filtered_data_right)
    else:
        interp_left = interp1d(filtered_data_left['W2'], filtered_data_left['qe_shifted'], kind='linear', fill_value='extrapolate')
        interp_right = interp1d(filtered_data_right['W2'], filtered_data_right['qe_shifted'], kind='linear', fill_value='extrapolate')
        qe_left_095 = float(interp_left(W2_split))
        qe_right_095 = float(interp_right(W2_split)) 
        if qe_left_095 != 0:
            scale_factor = qe_right_095 / qe_left_095
        else:
            scale_factor = 1.0
        filtered_data_left['sigtot_shifted'] *= scale_factor
        filtered_data_left['qe_shifted'] *= scale_factor
        combined_data.append(filtered_data_left)
        combined_data.append(filtered_data_right)

final_df = pd.concat(combined_data, ignore_index=True)
final_df.to_csv(data_result, index=False)