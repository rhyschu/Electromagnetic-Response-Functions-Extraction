import pandas as pd
import matplotlib.pyplot as plt
import os

# Only for Meziani Altemus Hotta

data1 = ['Meziani:1984is']
data2 = ['Altemus:1973']
E1 = [0.32]
E2 = [0.3488]
theta1 = [90]
theta2 = [90]

csv = 'df_Fe56_before.csv'
df = pd.read_csv(csv)

filtered_df1_before = df[((df['dataSet'].isin(data1)) if data1 else True) & ((df['E0'].isin(E1)) if E1 else True) & ((df['ThetaDeg'].isin(theta1)) if theta1 else True)]
filtered_df2_before = df[((df['dataSet'].isin(data2)) if data2 else True) & ((df['E0'].isin(E2)) if E2 else True) & ((df['ThetaDeg'].isin(theta2)) if theta2 else True)]

csv = 'df_Fe56_after.csv'
df = pd.read_csv(csv)

filtered_df1_after = df[((df['dataSet'].isin(data1)) if data1 else True) & ((df['E0'].isin(E1)) if E1 else True) & ((df['ThetaDeg'].isin(theta1)) if theta1 else True)]
filtered_df2_after = df[((df['dataSet'].isin(data2)) if data2 else True) & ((df['E0'].isin(E2)) if E2 else True) & ((df['ThetaDeg'].isin(theta2)) if theta2 else True)]

norm = filtered_df2_after['cross'].mean() / filtered_df2_before['cross'].mean()

os.makedirs("plots", exist_ok=True)
fig, axes = plt.subplots(1, 2, figsize=(12, 6), sharey=True)
axes[0].errorbar(filtered_df1_before['W2'], filtered_df1_before['cross'], yerr=filtered_df1_before['error'], fmt='o', label=f'{data1}, E={E1}, theta={theta1}')
axes[0].errorbar(filtered_df2_before['W2'], filtered_df2_before['cross'], yerr=filtered_df2_before['error'], fmt='o', label=f'{data2}, E={E2}, theta={theta2}')
axes[0].set_xlabel('W2')
axes[0].set_ylabel('cross')
axes[0].set_title("Before Norm")
axes[0].legend()
axes[0].grid(True)
axes[1].errorbar(filtered_df1_after['W2'], filtered_df1_after['cross'], yerr=filtered_df1_after['error'], fmt='o', label=f'{data1}, E={E1}, theta={theta1}')
axes[1].errorbar(filtered_df2_after['W2'], filtered_df2_after['cross'], yerr=filtered_df2_after['error'], fmt='o', label=f'{data2}, E={E2}, theta={theta2}')
axes[1].set_xlabel('W2')
axes[1].set_title(f'After Norm ({norm:.4f})')
axes[1].legend()
axes[1].grid(True)
plt.tight_layout()
if E1 and E2: 
    plt.savefig(f"plots/{E1[0]}_{E2[0]}.png")
else:
    plt.savefig(f"plots/{data1[0]}_{data2[0]}.png")
plt.show()
