import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

data = 'Data/df_Fe56.csv'
data_fit = 'Data/Fe56_Fit_LeftRightCombined.csv'
data_SuSAV2 = 'Data/df_Fe56_SuSAV2.csv'
data_Sheren = 'Data/df_Fe56_Sheren.csv'
pdf_file = 'Fe56_Cross.pdf'
elem = 'Fe56'
ex_cut_lower = 0
ex_cut_upper = 1000
multiplier = 26 / 6
A = 56
mass_nucleus = A * 0.931494
W2_split = 0.95**2
df = pd.read_csv(data)
df_fit = pd.read_csv(data_fit)
df_SuSAV2 = pd.read_csv(data_SuSAV2)
df_SuSAV2 = df_SuSAV2[df_SuSAV2['cross'] != 0]
df_SuSAV2.replace('#DIV/0!', pd.NA, inplace=True)
df_SuSAV2.dropna(inplace=True)
df_Sheren = pd.read_csv(data_Sheren)
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[1], x[2], x[0]))

with PdfPages(pdf_file) as pdf:
    for i in range(len(value_pairs) // 12 + 1):
        fig, axs = plt.subplots(ncols = 3, nrows = 4, figsize = (12, 12), dpi = 300) 
        for j, ax in enumerate(axs.flat):
            if i * 12 + j >= len(value_pairs):
                ax.axis('off')
                continue
            E0, ThetaDeg, dataSet = value_pairs[i * 12 + j]
            filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
            Z = filtered_data['Z'].iloc[0]
            A = filtered_data['A'].iloc[0]
            x = filtered_data['W2original']
            y = filtered_data['normCross']
            yerr = filtered_data['normCrossError']
            normalization = filtered_data['normalization'].iloc[0]
            filtered_data_fit = df_fit[(df_fit['E0'] == E0) & (df_fit['ThetaDeg'] == ThetaDeg) & (df_fit['dataSet'] == dataSet)]
            x_fit = filtered_data_fit['W2']
            y_fit = filtered_data_fit['sigtot_shifted']
            y_fit_QE = filtered_data_fit['qe_shifted']
            filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg) & (df_SuSAV2['dataSet'] == dataSet)]
            x_SuSAV2 = filtered_data_SuSAV2['W2original']
            y_SuSAV2 = filtered_data_SuSAV2['cross']
            filtered_data_Sheren = df_Sheren[(df_Sheren['E0'] == E0) & (df_Sheren['ThetaDeg'] == ThetaDeg) & (df_Sheren['dataSet'] == dataSet)]
            x_Sheren = filtered_data_Sheren['W2original']
            y_Sheren = filtered_data_Sheren['cross']
            
            ax.errorbar(x, y, yerr = yerr, fmt = '.', label = f'normCross', color = 'red', zorder = 2)
            ax.scatter(x_fit, y_fit, label = 'Christy-Bodek Fit Total', color = 'black', marker = '.', zorder = -2)
            ax.plot(x_fit, y_fit, color = 'black', linestyle = 'solid', alpha = 0.5, zorder = -2)
            ax.scatter(x_fit, y_fit_QE, label = 'Christy-Bodek Fit QE', color = 'black', marker = '.', zorder = -2)
            ax.plot(x_fit, y_fit_QE, color = 'black', linestyle = 'dashed', alpha = 0.5, zorder = -2)
            ax.scatter(x_SuSAV2, y_SuSAV2, label = 'SuSAV2', color = 'violet', marker = '.')
            ax.plot(x_SuSAV2, y_SuSAV2, color = 'violet', alpha = 0.5)
            ax.scatter(x_Sheren, y_Sheren, label = 'Sheren', color = 'deepskyblue', marker = '.')
            ax.plot(x_Sheren, y_Sheren, color = 'deepskyblue', alpha = 0.5)
            ax.axvline(x = W2_split, color = 'darkorange', linestyle = 'dashdot', lw = 1)
            ax.set_xlabel('$W^2 \ (GeV)$')
            ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
            ax.set_ylim(0, None)
            ax.set_title(f'{dataSet} {E0}$GeV$ {ThetaDeg}° (X {normalization})')
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0,0))
            ax.yaxis.set_major_formatter(formatter)
            if j == 0:
                ax.legend()
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)     
