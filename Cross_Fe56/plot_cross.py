import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

data = 'Data/Fe56.csv'
data_norm = 'Data/Fe56_Norm.csv'
data_fit = 'Data/Fe56_Fit.csv'
data_SuSAV2 = 'Data/Fe56_SuSAV2.csv'
data_Sheren = 'Data/Fe56_Sheren.csv'
pdf_file = 'Fe56_Cross.pdf'
elem = 'Fe56'
ex_cut_lower = 0
ex_cut_upper = 1000
multiplier = 26 / 6
A = 56
mass_nucleon = 0.938273
mass_nucleus = A * 0.931494
veff = 0.0089
nu_axis = True
W2_split = 0.95**2
df = pd.read_csv(data)
df["Veff"] = 0.0089
df["Ep_eff"] = df["E0"] - df["nu"] + df["Veff"]
df_norm = pd.read_csv(data_norm)
for _, row in df_norm.iterrows():
    mask = pd.Series([True] * len(df))
    if pd.notna(row['E0']):
        mask &= df['E0'] == row['E0']
    if pd.notna(row['ThetaDeg']):
        mask &= df['ThetaDeg'] == row['ThetaDeg']
    if pd.notna(row['dataSet']):
        mask &= df['dataSet'] == row['dataSet']
    df.loc[mask, 'normalization'] = row['normalization']
    df.loc[mask, 'normError'] = 0
    try:
        df.loc[mask, 'system_err'] = float(row['system_err'])
    except (ValueError, TypeError):
        df.loc[mask, 'system_err'] = np.nan
df.loc[df['dataSet'] == 'Meziani:1984is', 'system_err'] = np.where(df.loc[df['dataSet'] == 'Meziani:1984is', 'Ep_eff'] > 0.2, 0.037, 0.037 + 0.016 * (0.2 - df.loc[df['dataSet'] == 'Meziani:1984is', 'Ep_eff']) / 0.15)
df.loc[df['dataSet'] == 'Altemus:1973', 'system_err'] = np.where(df.loc[df['dataSet'] == 'Altemus:1973', 'Ep_eff'] > 0.2, 0.037, 0.037 + 0.016 * (0.2 - df.loc[df['dataSet'] == 'Altemus:1973', 'Ep_eff']) / 0.15)
df['normCross'] = df['cross'] * df['normalization']
df['error'] = np.sqrt(df['error']**2 + ((df['system_err'] * df['cross'])**2))
df['normCrossError'] = df['normCross'] * np.sqrt((df['error'] / df['cross'])**2 + (df['normError'] / df['normalization'])**2)
df_fit = pd.read_csv(data_fit)
df_SuSAV2 = pd.read_csv(data_SuSAV2)
df_SuSAV2 = df_SuSAV2[df_SuSAV2['cross'] != 0]
df_SuSAV2.replace('#DIV/0!', pd.NA, inplace=True)
df_SuSAV2.dropna(inplace=True)
df_Sheren = pd.read_csv(data_Sheren)
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[0], x[1]))

if not nu_axis:
    def cal_w2(df):
        df["ThetaRad"] = df["ThetaDeg"] * np.pi / 180
        df["sin2(T/2)"] = (np.sin(df["ThetaRad"] / 2))**2
        df["Q2"] = 4 * df["E0"] * (df["E0"] - df["nu"]) * df["sin2(T/2)"]
        df["W2original"] = mass_nucleon**2 + 2 * mass_nucleon * df["nu"] - df["Q2"]
    cal_w2(df)
    cal_w2(df_fit)
    cal_w2(df_SuSAV2)
    cal_w2(df_Sheren)

with PdfPages(pdf_file) as pdf:
    for i in range(len(value_pairs) // 12 + 1):
        fig, axs = plt.subplots(ncols = 3, nrows = 4, figsize = (18, 18), dpi = 600) 
        for j, ax in enumerate(axs.flat):
            if i * 12 + j >= len(value_pairs):
                ax.axis('off')
                continue
            E0, ThetaDeg, dataSet = value_pairs[i * 12 + j]
            filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
            filtered_data = filtered_data.sort_values(by = 'nu')
            x = filtered_data['nu'] if nu_axis else filtered_data['W2original']
            y = filtered_data['normCross']
            yerr = filtered_data['normCrossError']
            normalization = filtered_data['normalization'].iloc[0]
            filtered_data_fit = df_fit[(df_fit['E0'] == E0) & (df_fit['ThetaDeg'] == ThetaDeg)]
            filtered_data_fit = filtered_data_fit.sort_values(by = 'nu')
            x_fit = filtered_data_fit['nu'] if nu_axis else filtered_data_fit['W2original']
            y_fit = filtered_data_fit['sigtot']
            filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg)]
            filtered_data_SuSAV2 = filtered_data_SuSAV2.sort_values(by = 'nu')
            x_SuSAV2 = filtered_data_SuSAV2['nu'] if nu_axis else filtered_data_SuSAV2['W2original']
            y_SuSAV2 = filtered_data_SuSAV2['cross']
            filtered_data_Sheren = df_Sheren[(df_Sheren['E0'] == E0) & (df_Sheren['ThetaDeg'] == ThetaDeg)]
            filtered_data_Sheren = filtered_data_Sheren.sort_values(by = 'nu')
            x_Sheren = filtered_data_Sheren['nu'] if nu_axis else filtered_data_Sheren['W2original']
            y_Sheren = filtered_data_Sheren['cross']
            ax.errorbar(x, y, yerr=yerr, fmt='.', label='normCross', color='blue', markersize=8, capsize=0, alpha=1.0, zorder=1)
            ax.plot(x_fit, y_fit, label='Christy-Bodek Fit', color='red', linestyle='solid', linewidth=2, alpha=0.5, zorder=2)
            ax.scatter(x_Sheren, y_Sheren, label='Sheren', color='saddlebrown', marker='D', s=12, linewidth=0, alpha=1.0, zorder=3)
            ax.scatter(x_SuSAV2, y_SuSAV2, label='SuSAV2', color='lawngreen', marker='s', s=12, linewidth=0, alpha=1.0, zorder=4)
            if not nu_axis:
                ax.axvline(x = W2_split, color = 'darkorange', linestyle = 'dashdot', lw = 1)
            if nu_axis:
                ax.set_xlabel('$\\nu \ (GeV)$')
            else:
                ax.set_xlabel('$W^2 (GeV^2)$')
            ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
            ax.set_ylim(0, None)
            ax.set_title(f'{dataSet} {E0}$GeV$ {ThetaDeg}° (X {normalization:.4f})')
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0,0))
            ax.yaxis.set_major_formatter(formatter)
            if j == 0:
                ax.legend()
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)   
