import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import ScalarFormatter

data = 'df_Fe56.csv'
data_fit = 'Fe56_Fit.csv'
data_SuSAV2 = 'df_Fe56_SuSAV2_Extraction.csv'
pdf_file = "Fe56_Cross_Section.pdf"
elem= '56Fe'
elem_dict = {
    "1H": {"A": 1, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 1/6, "QENS_shift": 0, "NS_factor": 1},
    "2H": {"A": 2, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 1/6, "QENS_shift": 0, "NS_factor": 1},
    "12C": {"A": 12, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 1, "QENS_shift": 0, "NS_factor": 1},
    "40Ca": {"A": 40, "ex_cut_lower": 0.03, "ex_cut_upper": 1000, "multiplier": 20/6, "QENS_shift": 0, "NS_factor": 1},
    "56Fe": {"A": 56, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 26/6, "QENS_shift": 0.013, "NS_factor": 0.5}
}
mass_nucleus = elem_dict[elem]["A"] * 0.931494
x_axis_nu = False
df = pd.read_csv(data)
df_fit = pd.read_csv(data_fit)
df_SuSAV2 = pd.read_csv(data_SuSAV2)
df_SuSAV2 = df_SuSAV2[df_SuSAV2['cross'] != 0]
df_SuSAV2.replace('#DIV/0!', pd.NA, inplace=True)
df_SuSAV2.dropna(inplace=True)
value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[1], x[0]))

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
                if x_axis_nu:
                    x = filtered_data['nu']
                else:
                    x = filtered_data['Ex']
                y = filtered_data['normCross']
                yerr = filtered_data['normCrossError']
                normaliztion = filtered_data['normalization'].iloc[0]
                filtered_data_fit = df_fit[(df_fit['E0'] == E0) & (df_fit['ThetaDeg'] == ThetaDeg) & (df_fit['dataSet'] == dataSet)]
                if x_axis_nu:
                    x_fit = filtered_data_fit['nu']
                else:
                    x_fit = filtered_data_fit['Ex']
                y_fit = filtered_data_fit['sigtot_shifted']
                y_fit_QE = filtered_data_fit['qe_shifted'] + filtered_data_fit['mec_shifted']
                filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg) & (df_SuSAV2['dataSet'] == dataSet)]
                if x_axis_nu:
                    x_SuSAV2 = filtered_data_SuSAV2['nu']
                else:
                    x_SuSAV2 = filtered_data_SuSAV2['Ex']
                y_SuSAV2 = filtered_data_SuSAV2['cross']
                
                ax.errorbar(x, y, yerr=yerr, fmt='.', label=f'normCross', color='blue', zorder=-1)
                # ax.scatter(x_fit, y_fit, label='Christy-Bodek Fit Total', color='red', marker='.')
                # ax.plot(x_fit, y_fit, color='red', alpha=0.5)
                ax.scatter(x_fit, y_fit_QE, label='Christy-Bodek Fit QE', color='red', marker='.')
                ax.plot(x_fit, y_fit_QE, color='red', alpha=0.5, linestyle='--')
                ax.scatter(x_SuSAV2, y_SuSAV2, label='SuSAV2', color='lightgreen', marker='.')
                ax.plot(x_SuSAV2, y_SuSAV2, color='lightgreen', alpha=0.5)
                if x_axis_nu:
                    ax.set_xlabel('$\\nu \ (GeV)$')
                else:
                    ax.set_xlabel('$Ex \ (GeV)$')
                ax.set_ylabel('$\\frac{d^2 \sigma}{d\Omega d\\nu} (nb/sr/GeV)$')
                ax.set_ylim(0, None)
                ax.set_title(f'{dataSet} {E0}$GeV$ {ThetaDeg}° (X {normaliztion})')
                formatter = ScalarFormatter(useMathText=True)
                formatter.set_scientific(True)
                formatter.set_powerlimits((0,0))
                ax.yaxis.set_major_formatter(formatter)
                ax.legend() 
            plt.tight_layout()
            pdf.savefig(fig)
            plt.close(fig)     
