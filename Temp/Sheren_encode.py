import pandas as pd
import numpy as np
# Define the input and output file paths
input_file = 'Sheren_Raw.csv'
output_file = 'C12_Sheren.csv'
mass_nucleon = 0.938273
df = pd.read_csv(input_file)

df['nu'] = (df["W2"] + df["Q2"] - mass_nucleon**2) / (2 * mass_nucleon)
df['RLerr'] = np.sqrt(df['RLerr(pt-pt)']**2 + df['RLerr_th']**2 + df['RLerr_rad']**2)
df['RTerr'] = np.sqrt(df['RTerr(pt-pt)']**2 + df['RTerr_th']**2 + df['RTerr_rad']**2)
# Save the DataFrame to CSV
df.to_csv(output_file, index=False)

print(f"Data successfully converted to '{output_file}'.")
