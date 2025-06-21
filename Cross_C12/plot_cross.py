import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

data = 'Data/df_C12.csv'
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
value_pairs = sorted(value_pairs_raw, key=lambda x: (x[1], x[0]))

with PdfPages(pdf_file) as pdf:
    for i in range(len(value_pairs) // 12 + 1):
        fig, axs = plt.subplots(ncols=3, nrows=4, figsize=(12, 12), dpi=200) 
        for j, ax in enumerate(axs.flat):
            if i * 12 + j >= len(value_pairs):
                ax.axis('off')
                continue
            E0, ThetaDeg = value_pairs[i * 12 + j]
            filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg)]
            x = filtered_data['nu']
            y = filtered_data['normCross']
            yerr = filtered_data['normCrossError']
            filtered_data_fit = df_fit[(df_fit['E0'] == E0) & (df_fit['ThetaDeg'] == ThetaDeg)]
            filtered_data_fit = filtered_data_fit.sort_values(by='nu')
            x_fit = filtered_data_fit['nu']
            y_fit = filtered_data_fit['sigtot']
            filtered_data_GENIE = df_GENIE[(df_GENIE['E0'] == E0) & (df_GENIE['ThetaDeg'] == ThetaDeg)]
            filtered_data_GENIE = filtered_data_GENIE.sort_values(by='nu')
            x_GENIE = filtered_data_GENIE['nu']
            y_GENIE = filtered_data_GENIE['cross']
            filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg)]
            filtered_data_SuSAV2 = filtered_data_SuSAV2.sort_values(by='nu')
            x_SuSAV2 = filtered_data_SuSAV2['nu']
            y_SuSAV2 = filtered_data_SuSAV2['cross']
            
            ax.errorbar(x, y, yerr=yerr, fmt='.', label='normCross', color='red', zorder=-1)
            ax.plot(x_fit, y_fit, label='Christy-Bodek Fit Total', color='black', alpha=0.5)
            ax.scatter(x_GENIE, y_GENIE, label='GENIE', color='deepskyblue', marker='.')
            ax.plot(x_GENIE, y_GENIE, color='deepskyblue', alpha=0.5)
            ax.scatter(x_SuSAV2, y_SuSAV2, label='SuSAV2', color='lightgreen', marker='.')
            ax.plot(x_SuSAV2, y_SuSAV2, color='lightgreen', alpha=0.5)
            ax.set_xlabel('$\\nu \ (GeV)$')
            ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
            ax.set_ylim(0, None)
            ax.set_title(f'{E0}$GeV$ {ThetaDeg}°')
            formatter = ScalarFormatter(useMathText=True)
            formatter.set_scientific(True)
            formatter.set_powerlimits((0,0))
            ax.yaxis.set_major_formatter(formatter)
            ax.legend()
            ax.legend(fontsize=6)
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)
