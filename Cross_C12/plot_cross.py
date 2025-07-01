import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

dataSet_to_name = {1:"Barreau:1983ht", 2:"O'Connell:198", 3:"Sealock:1989nx", 4:"Baran:1988tw", 5:"Bagdasaryan:1988hp", 6:"Dai - HallA:2019da", 
                   7:"Arrington:1995hs", 8:"Day:1993md", 9:"Arrington:1998psnoCC", 10:"Gaskell:2008", 11:"Whitney:1974hr", 12:"AlsamiJan05", 
                   13:"VaheJun07", 14:"Gomez74", 15:"Fomin", 16:"Yamaguchi73", 17:"Ryan84", 18:"Cyzyk:1963zz", 
                   19:"Bounin63", 20:"Photo-Daphne", 21:"Antony-Spies:1970jjs",22:"Goldemberg64", 23:"DeForrest65", 24:"Mihovilovic:2024ymj", 25:"CLAS-e4nu",26:"GarinoBates:1992"}
dataSet_to_normalization = {1: 0.95971, 2: 0.96416, 3: 1.0744, 4: 0.99482, 5: 0.93381, 6: 1.0126, 
                            7: 0.96716, 8: 1.0238, 9: 0.97904, 10: 0.99064, 11: 0.98384, 12: 1.0000, 
                            13: 1.0163, 14: 1.0300, 15: 1.0190, 16: 0.95853, 17: 1.0174, 18: 1.0168, 
                            19: 1.0794, 20: 1.0000, 21: 0.9500, 22: 1.1095, 23: 0.9310, 24: 1.0019, 
                            25: 0.8500, 26: 1.0000, 33: 0.9980, 34: 0.9677, 35: 0.9561}
dataSet_to_normError = {1: 0.62926E-02, 2: 0.12908E-01, 3: 0.80983E-02, 4: 0.69809E-02, 5: 0.16758E-01, 6: 0.92261E-02, 
                        7: 0.15546E-01, 8: 0.65203E-02, 9: 0.55606E-02, 10: 0.75245E-02, 11: 0.25318E-01, 12: 0.0, 
                        13: 0.17632E-02, 14: 0.91993E-02, 15: 0.63181E-02, 16: 0.25582E-01, 17: 0.42184E-01, 18: 0.68067E-01, 
                        19: 0.35847E-01, 20: 0.0, 21: 0.25, 22: 0.1, 23: 0.1, 24: 0.184E-01, 
                        25: 0.02, 26: 0.02, 33: 0.415E-01, 34: 0.173E-01, 35: 0.231E-01}
data = 'Data/C12.csv'
data_fit = 'Data/C12_Fit.csv'
data_GENIE = 'Data/C12_Genie.csv'
data_SuSAV2 = 'Data/C12_SuSAV2.csv'
pdf_file = 'C12_Cross.pdf'
elem = 'C12'
ex_cut_lower = 0
ex_cut_upper = 1000
A = 12
mass_nucleus = A * 0.931494
df = pd.read_csv(data)
df["normalization"] = df["dataSet"].map(dataSet_to_normalization)
df["normError"] = df["dataSet"].map(dataSet_to_normError)
df['system_err'] = 0.02
df['normCross'] = df['cross'] * df['normalization']
df['error'] = np.sqrt(df['error']**2 + ((df['system_err'] * df['cross'])**2))
df['normCrossError'] = df['normCross'] * np.sqrt((df['error'] / df['cross'])**2 + (df['normError'] / df['normalization'])**2)
df_fit = pd.read_csv(data_fit)
df_GENIE = pd.read_csv(data_GENIE)
df_GENIE = df_GENIE[df_GENIE['cross'] > 0]
df_SuSAV2 = pd.read_csv(data_SuSAV2)
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[0], x[1]))

with PdfPages(pdf_file) as pdf:
    for i in range(len(value_pairs) // 12 + 1):
        fig, axs = plt.subplots(ncols = 3, nrows = 4, figsize = (18, 18), dpi = 300) 
        for j, ax in enumerate(axs.flat):
            if i * 12 + j >= len(value_pairs):
                ax.axis('off')
                continue
            E0, ThetaDeg, dataSet = value_pairs[i * 12 + j]
            filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
            filtered_data = filtered_data.sort_values(by = 'nu')
            x = filtered_data['nu']
            y = filtered_data['normCross']
            yerr = filtered_data['normCrossError']
            dataSetName = dataSet_to_name[dataSet]
            normalization = dataSet_to_normalization[dataSet]
            filtered_data_fit = df_fit[(df_fit['E0'] == E0) & (df_fit['ThetaDeg'] == ThetaDeg)]
            filtered_data_fit = filtered_data_fit.sort_values(by = 'nu')
            x_fit = filtered_data_fit['nu']
            y_fit = filtered_data_fit['sigtot']
            filtered_data_GENIE = df_GENIE[(df_GENIE['E0'] == E0) & (df_GENIE['ThetaDeg'] == ThetaDeg)]
            filtered_data_GENIE = filtered_data_GENIE.sort_values(by = 'nu')
            x_GENIE = filtered_data_GENIE['nu']
            y_GENIE = filtered_data_GENIE['cross']
            filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg)]
            filtered_data_SuSAV2 = filtered_data_SuSAV2.sort_values(by = 'nu')
            x_SuSAV2 = filtered_data_SuSAV2['nu']
            y_SuSAV2 = filtered_data_SuSAV2['cross']
            
            ax.errorbar(x, y, yerr=yerr, fmt='.', label='normCross', color='blue', markersize=5, capsize=0, alpha=1.0, zorder=1)
            ax.plot(x_fit, y_fit, label='Christy-Bodek Fit', color='red', linestyle='solid', linewidth=1, alpha=0.5, zorder=2)
            ax.scatter(x_GENIE, y_GENIE, label='GENIE', color='orange', marker='.', s=15, alpha=0.5, zorder=3)
            ax.scatter(x_SuSAV2, y_SuSAV2, label='SuSAV2', color='green', marker='.', s=15, alpha=0.8, zorder=4)
            ax.set_xlabel('$\\nu \ (GeV)$')
            ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
            ax.set_ylim(0, None)
            ax.set_title(f'{int(dataSet)}:{dataSetName} {E0}$GeV$ {ThetaDeg}° (X {normalization:.4f})')
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0,0))
            ax.yaxis.set_major_formatter(formatter)
            if j == 0:
                ax.legend()
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)
