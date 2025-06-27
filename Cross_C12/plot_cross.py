import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

dataSet_to_name = {1:"Barreau:1983ht", 2:"O'Connell:198", 3:"Sealock:1989nx", 4:"Baran:1988tw", 5:"Bagdasaryan:1988hp", 6:"Dai - HallA:2019da", 
                   7:"Arrington:1995hs", 8:"Day:1993md", 9:"Arrington:1998psnoCC", 10:"Gaskell:2008", 11:"Whitney:1974hr", 12:"AlsamiJan05", 
                   13:"VaheJun07", 14:"Gomez74", 15:"Fomin", 16:"Yamaguchi73", 17:"Ryan84", 18:"Cyzyk:1963zz", 
                   19:"Bounin63", 20:"Photo-Daphne", 21:"Antony-Spies:1970jjs",22:"Goldemberg64", 23:"DeForrest65", 24:"Mihovilovic:2024ymj", 25:"CLAS-e4nu",26:"GarinoBates:1992"}
data = 'Data/df_C12_Refined.csv'
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
df_fit = pd.read_csv(data_fit)
df_GENIE = pd.read_csv(data_GENIE)
df_SuSAV2 = pd.read_csv(data_SuSAV2)
value_pairs_raw = set()
value_pairs_raw.update((row["E0"], row["ThetaDeg"]) for _, row in df.iterrows())
value_pairs_raw.update((row["E0"], row["ThetaDeg"]) for _, row in df_GENIE.iterrows())
value_pairs_raw.update((row["E0"], row["ThetaDeg"]) for _, row in df_SuSAV2.iterrows())
value_pairs = sorted(value_pairs_raw, key=lambda x: (x[0], x[1]))

with PdfPages(pdf_file) as pdf:
    for i in range(len(value_pairs) // 12 + 1):
        fig, axs = plt.subplots(ncols = 3, nrows = 4, figsize = (12, 12), dpi = 300) 
        for j, ax in enumerate(axs.flat):
            if i * 12 + j >= len(value_pairs):
                ax.axis('off')
                continue
            E0, ThetaDeg = value_pairs[i * 12 + j]
            filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg)]
            filtered_data = filtered_data.sort_values(by = 'nu')
            x = filtered_data['nu']
            y = filtered_data['normCross']
            yerr = filtered_data['normCrossError']
            dataSet = -1
            if not filtered_data.empty:
                dataSet = filtered_data['dataSet'].iloc[0]
            dataSetName = dataSet_to_name.get(dataSet, 'N/A')
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
            
            ax.errorbar(x, y, yerr = yerr, fmt = '.', label = 'normCross', color = 'red', zorder = 2)
            ax.plot(x_fit, y_fit, label = 'Christy-Bodek Fit Total', color = 'black', linestyle = 'solid', zorder = -2)
            ax.scatter(x_GENIE, y_GENIE, label = 'GENIE', color = 'deepskyblue', marker = '.', zorder = 1)
            ax.plot(x_GENIE, y_GENIE, color = 'deepskyblue', alpha = 0.5)
            ax.scatter(x_SuSAV2, y_SuSAV2, label = 'SuSAV2', color = 'violet', marker = '.', zorder = -1)
            ax.plot(x_SuSAV2, y_SuSAV2, color = 'violet', alpha = 0.5)
            ax.set_xlabel('$\\nu \ (GeV)$')
            ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
            ax.set_ylim(0, None)
            ax.set_title(f'{dataSet}:{dataSetName} {E0}$GeV$ {ThetaDeg}°')
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0,0))
            ax.yaxis.set_major_formatter(formatter)
            if j == 0:
                ax.legend()
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)
