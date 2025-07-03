import pandas as pd
import numpy as np

data = 'Data/C12.csv'
data_GENIE = 'Data/C12_Genie.csv'
output_merged = 'Data/C12_merged.csv'

df = pd.read_csv(data)
df_GENIE = pd.read_csv(data_GENIE)

generated_rows = []

value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[0], x[1]))
for value_pair in value_pairs:
    E0, ThetaDeg, dataSet = value_pair
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
    filtered_data_GENIE = df_GENIE[(df_GENIE['E0'] == E0) & (df_GENIE['ThetaDeg'] == ThetaDeg)]
    if filtered_data.empty and filtered_data_GENIE.empty:
        continue

    min_nu = min(filtered_data['nu'].min() if not filtered_data.empty else float('inf'), filtered_data_GENIE['nu'].min() if not filtered_data_GENIE.empty else float('inf'))
    max_nu = max(filtered_data['nu'].max() if not filtered_data.empty else float('-inf'), filtered_data_GENIE['nu'].max() if not filtered_data_GENIE.empty else float('-inf'))

    nu_fine = np.arange(min_nu, min(0.05, max_nu), 0.0002)
    nu_coarse = np.arange(max(min(0.05, max_nu), min_nu), max_nu + 0.005, 0.01)
    nu_values = np.unique(np.concatenate([nu_fine, nu_coarse]))

    for nu in nu_values:
        row = {
            "Z": 6,
            "A": 12,
            "E0": E0,
            "ThetaDeg": ThetaDeg,
            "nu": round(nu, 6),
            "cross": 1,
            "error": 0,
            "dataSet": -1
        }
        generated_rows.append(row)

# Convert to DataFrame and save
data_merged = pd.DataFrame(generated_rows)
data_merged.to_csv(output_merged, index = False)
print(f"Generated {len(data_merged)} rows and saved to {output_merged}")
