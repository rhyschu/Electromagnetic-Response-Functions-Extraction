import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

data = '56Fe.csv'
data_fit = '56Fe_Fit.csv'
data_SuSAV2 = '56Fe_SuSAV2.csv'
pdf_file = "Fe56_Cross_Section.pdf"
elem= '56Fe'
elem_dict = {
    "1H": {"A": 1, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 1/6, "QENS_shift": 0, "NS_factor": 1},
    "2H": {"A": 2, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 1/6, "QENS_shift": 0, "NS_factor": 1},
    "12C": {"A": 12, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 1, "QENS_shift": 0, "NS_factor": 1},
    "40Ca": {"A": 40, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 20/6, "QENS_shift": 0, "NS_factor": 1},
    "56Fe": {"A": 56, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 26/6, "QENS_shift": 0.013, "NS_factor": 0.5}
}
mass_nucleus = elem_dict[elem]["A"]*0.931494
df = pd.read_csv(data)
df_fit = pd.read_csv(data_fit)
df_SuSAV2 = pd.read_csv(data_SuSAV2)
df_SuSAV2 = df_SuSAV2[df_SuSAV2['cross'] != 0]
df_SuSAV2.replace('#DIV/0!', pd.NA, inplace=True)
df_SuSAV2.dropna(inplace=True)
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[0], x[1]))

with PdfPages(pdf_file) as pdf:
        for i in range(len(value_pairs) // 12 + 1):
            fig, axs = plt.subplots(ncols=3, nrows=4, figsize=(12, 12), dpi=200) 
            for j, ax in enumerate(axs.flat):
                if i * 12 + j >= len(value_pairs):
                    ax.axis('off')
                    continue
                E0, ThetaDeg, dataSet = value_pairs[i * 12 + j]
                filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet)]
                Z = filtered_data['Z'].iloc[0]
                A = filtered_data['A'].iloc[0]
                x = filtered_data['nu']  
                y = filtered_data['cross']  
                yerr = filtered_data['error']
                filtered_data_fit = df_fit[(df_fit['E0'] == E0) & (df_fit['ThetaDeg'] == ThetaDeg) & (df_fit['dataSet'] == dataSet)] 
                x_fit = filtered_data_fit['nu']  
                y_fit = filtered_data_fit['sigtot_shifted']
                filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg) & (df_SuSAV2['dataSet'] == dataSet)]
                x_SuSAV2 = filtered_data_SuSAV2['nu']
                y_SuSAV2 = filtered_data_SuSAV2['cross']

                ax.errorbar(x, y, yerr=yerr, fmt='.', label=f'$E_0=${E0}$GeV$\n$\\theta=${ThetaDeg}°', color='blue', zorder=-1)
                ax.scatter(x_fit, y_fit, label='Christy-Bodek Fit', color='red', marker='.')
                ax.plot(x_fit, y_fit, color='red', alpha=0.5)
                ax.scatter(x_SuSAV2, y_SuSAV2, label='SuSAV2', color='lightgreen', marker='.')
                ax.plot(x_SuSAV2, y_SuSAV2, color='lightgreen', alpha=0.5)
                ax.set_xlabel('$\\nu \ (GeV)$')    
                ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
                ax.set_ylim(0, None)
                ax.set_title(f'{dataSet} {E0}$GeV$ {ThetaDeg}°')
                formatter = ScalarFormatter(useMathText=True)
                formatter.set_scientific(True)
                formatter.set_powerlimits((0,0))
                ax.yaxis.set_major_formatter(formatter)
                ax.legend() 
            plt.tight_layout()
            pdf.savefig(fig)
            plt.close(fig)     
