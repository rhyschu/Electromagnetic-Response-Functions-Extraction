#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

df1 = pd.read_csv('response.csv')
df1.columns = df1.columns.str.strip()

#change
add_error = 0.05
E1 = 0.12
theta1 = 140
data1 = 'Meziani:1984is'
E2 =  0.1003
theta2 = 140
data2 = 'Altemus:1973'
Z = 26
A = 56

#change for selected points
min_ex = 0.035
max_ex = 100
min_w2 = 0
max_w2 = 100

df1['ratio'] = pd.to_numeric(df1['ratio'], errors='coerce')
df1 = df1.dropna(subset=['ratio'])

filtered_df1 = df1[(df1['E0'] == E1) & (df1['theta'] == theta1)& (df1['dataSet'] == data1)].copy()
filtered_df1['ratio error'] = filtered_df1['error'] / filtered_df1['scaled_sigtot_Fe56']
filtered_df1['ratio error'] = np.sqrt(filtered_df1['ratio error']**2 + (add_error * filtered_df1['ratio'])**2)


df2 = pd.read_csv('response.csv')
df2.columns = df2.columns.str.strip()

df2['ratio'] = pd.to_numeric(df2['ratio'], errors='coerce')
df2 = df2.dropna(subset=['ratio'])

filtered_df2 = df2[(df2['E0'] == E2) & (df2['theta'] == theta2)& (df2['dataSet'] == data2)].copy()
filtered_df2['ratio error'] = filtered_df2['error'] / filtered_df2['scaled_sigtot_Fe56']
filtered_df2['ratio error'] = np.sqrt(filtered_df2['ratio error']**2 + (add_error * filtered_df2['ratio'])**2)

fig, axs = plt.subplots(1, 2, figsize=(12, 6))
axs[0].errorbar(filtered_df1['nu-nuel'], filtered_df1['ratio'], yerr=filtered_df1['ratio error'], fmt='o', label=f'{data1}, E={E1}, theta={theta1}')
axs[0].errorbar(filtered_df2['nu-nuel'], filtered_df2['ratio'], yerr=filtered_df2['ratio error'], fmt='o', label=f'{data2}, E={E2}, theta={theta2}')
axs[0].set_xlabel('Ex')
axs[0].set_ylabel('ratio')
axs[0].set_ylim(0,2)
axs[0].set_title(f"[Ex]Experiment:{data1} vs {data2}; Z:{Z} A:{A}")
axs[0].legend()
axs[0].grid(True)
axs[1].errorbar(filtered_df1['w2'], filtered_df1['ratio'], yerr=filtered_df1['ratio error'], fmt='o', label=f'{data1}, E={E1}, theta={theta1}')
axs[1].errorbar(filtered_df2['w2'], filtered_df2['ratio'], yerr=filtered_df2['ratio error'], fmt='o', label=f'{data2}, E={E2}, theta={theta2}')
axs[1].set_xlabel('w2')
axs[1].set_ylabel('ratio')
axs[1].set_ylim(0,2)
axs[1].set_title(f"[W2]Experiment:{data1} vs {data2}; Z:{Z} A:{A}")
axs[1].legend()
axs[1].grid(True)
plt.tight_layout()
plt.show()

#Filter

range_df1 = filtered_df1[(filtered_df1['nu-nuel'] > min_ex) & (filtered_df1['nu-nuel'] < max_ex) & (filtered_df1['w2'] > min_w2) & (filtered_df1['w2'] < max_w2)].copy()
range_df2 = filtered_df2[(filtered_df2['nu-nuel'] > min_ex) & (filtered_df2['nu-nuel'] < max_ex) & (filtered_df2['w2'] > min_w2) & (filtered_df2['w2'] < max_w2)].copy()
print("N points 1:", range_df1.shape[0])
print("N points 2:", range_df2.shape[0])

#W2 RATIO
interp_ratio2 = np.interp(range_df1['w2'], range_df2['w2'], range_df2['ratio'])
interp_error2 = np.interp(range_df1['w2'], range_df2['w2'], range_df2['ratio error'])

ratio_vals = range_df1['ratio'] / interp_ratio2
ratio_errs = np.sqrt(
    (range_df1['ratio error'] / interp_ratio2)**2
    + (range_df1['ratio'] * interp_error2 / (interp_ratio2**2))**2
)

mask = (
    np.isfinite(ratio_vals)
    & np.isfinite(ratio_errs)
    & (ratio_errs > 0)
    & (interp_ratio2 != 0)
)
ratio_vals = ratio_vals[mask]
ratio_errs = ratio_errs[mask]

if len(ratio_vals) == 0:
    print("No valid points remain after filtering; cannot compute scale factor.")
    k = np.nan
    k_err = np.nan
else:
    weights = 1.0 / (ratio_errs**2)
    sum_w   = np.sum(weights)
    if sum_w == 0:
        print("Sum of weights is zero; cannot compute scale factor.")
        k = np.nan
        k_err = np.nan
    else:
        k     = np.sum(ratio_vals * weights) / sum_w
        k_err = 1.0 / np.sqrt(sum_w)
    print(f"W2: Best-fit scale factor k = {k:.4f} ± {k_err:.4f}")

plt.figure(figsize=(12, 6))
plt.errorbar(range_df1['w2'], range_df1['ratio'], yerr=range_df1['ratio error'], fmt='o', label=f'{data1}, E={E1}, theta={theta1}')
plt.errorbar(range_df2['w2'], range_df2['ratio'], yerr=range_df2['ratio error'], fmt='o', label=f'{data2}, E={E2}, theta={theta2}')
plt.xlabel('w2')
plt.ylabel('ratio')
plt.title(f"[W2] Experiment: {data1} vs {data2}, <R> = {k:.4f} ± {k_err:.4f}")
plt.legend()
plt.grid(True)
plt.show()
