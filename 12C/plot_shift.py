import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import subprocess
from matplotlib.backends.backend_pdf import PdfPages

data = '56Fe.csv'
data_SuSAV2 = '56Fe_SuSAV2.csv'
element = '56Fe'
element_dict = {
    "12C": {"A": 12, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1},
    "40Ca": {"A": 40, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 20/6},
    "56Fe": {"A": 56, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 26/6}
}
mass_nucleus = element_dict[element]["A"]*0.931494
pdf = PdfPages("Fe56_Cross_Section.pdf")
df = pd.read_csv(data)
df["ThetaRad"] = df["ThetaDeg"]*np.pi/180
df["sin2(T/2)"] = (np.sin(df["ThetaRad"]/2))**2
df["nuel"] = df["E0"] - df["E0"]/(1 + 2*df["E0"]*df["sin2(T/2)"]/mass_nucleus)
df["Ex"] = df["nu"] - df["nuel"]
df_SuSAV2 = pd.read_csv(data_SuSAV2)
df_SuSAV2 = df_SuSAV2[df_SuSAV2['cross'] != 0]
df_SuSAV2["ThetaRad"] = df_SuSAV2["ThetaDeg"]*np.pi/180
df_SuSAV2["sin2(T/2)"] = (np.sin(df_SuSAV2["ThetaRad"]/2))**2
df_SuSAV2["nuel"] = df_SuSAV2["E0"] - df_SuSAV2["E0"]/(1 + 2*df_SuSAV2["E0"]*df_SuSAV2["sin2(T/2)"]/mass_nucleus)
df_SuSAV2["Ex"] = df_SuSAV2["nu"] - df_SuSAV2["nuel"]

def plot_cross_section(E0, ThetaDeg, dataSet):
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet) & (df['Ex'] > element_dict[element]["ex_cut_lower"]) & (df['Ex'] < element_dict[element]["ex_cut_upper"]) & (df['nu'] > 0.013)]
    Z = filtered_data['Z'].iloc[0]
    A = filtered_data['A'].iloc[0]
    x = filtered_data['nu']  
    y = filtered_data['cross']  
    yerr = filtered_data['error']  
    x_fit = []
    y_fit = []
    with open('output1.txt', 'r') as file1, open('output2.txt', 'r') as file2:
        for line1, line2 in zip(file1, file2):
            columns1 = line1.split()
            columns2 = line2.split()       
            x_fit.append(float(columns1[2]))  
            y_fit.append((float(columns1[6]) - float(columns1[7]) + float(columns2[7]) - float(columns1[10]) + float(columns2[10]) / 2)*1000*12*element_dict[element]["multiplier"])
    if len(x) != len(x_fit):
        print(f'{dataSet} {E0} {ThetaDeg}: nu and nu_fit have different dimensions.')
        return
    R = (y / y_fit)
    Rerr = (yerr / y_fit)
    valid_indices = np.isfinite(R) & np.isfinite(Rerr) & (Rerr > 0)
    x_valid = x[valid_indices].reset_index(drop=True)
    R = R[valid_indices].reset_index(drop=True)
    Rerr = Rerr[valid_indices].reset_index(drop=True)
    average = sum(y)/sum(y_fit)
    average_error = average*sum(yerr)/sum(y)

    # Weighted Average Version
    # sum1 = 0
    # sum2 = 0
    # for i in range(len(R)):
    #     sum1 += R[i] / (Rerr[i] ** 2)
    #     sum2 += 1 / (Rerr[i] ** 2)
    # average = sum1 / sum2
    # average_error = (1 / sum2) ** 0.5
    # file_exists = os.path.isfile("weighted_average.csv")
    # with open("weighted_average.csv", mode="a", newline="") as file:
    #     writer = csv.writer(file)
    #     if not file_exists:
    #         writer.writerow(["dataSet", "E0", "ThetaDeg", "Z", "A", "weighted_average", "weighted_average_error"])
    #     writer.writerow([dataSet, E0, ThetaDeg, Z, A, weighted_average, weighted_average_error])

    # SuSAV2
    filtered_data_SuSAV2 = df_SuSAV2[(df_SuSAV2['E0'] == E0) & (df_SuSAV2['ThetaDeg'] == ThetaDeg) & (df_SuSAV2['dataSet'] == dataSet) & (df_SuSAV2['Ex'] > element_dict[element]["ex_cut_lower"]) & (df_SuSAV2['Ex'] < element_dict[element]["ex_cut_upper"]) & (df_SuSAV2['nu'] > 0.013)]
    x_SuSAV2 = pd.to_numeric(filtered_data_SuSAV2['nu']) 
    y_SuSAV2 = pd.to_numeric(filtered_data_SuSAV2['cross'])
    R_SuSAV2 = pd.to_numeric(filtered_data_SuSAV2['ratio'])
    Rerr_SuSAV2 = pd.to_numeric(filtered_data_SuSAV2['ratio_error'])
    average_SuSAV2 = sum(y)/sum(y_SuSAV2)
    average_error_SuSAV2 = average_SuSAV2*sum(yerr)/sum(y)

    fig, axes = plt.subplots(1, 2, figsize=(10, 6))
    axes[0].errorbar(x, y, yerr=yerr, fmt='o', markersize=4, color='red', label='Data')
    axes[0].scatter(x_fit, y_fit, s=6, color='blue')
    axes[0].plot(x_fit, y_fit, color='blue', label='Scaled 12C')
    axes[0].scatter(x_SuSAV2, y_SuSAV2, s=6, color='silver')
    axes[0].plot(x_SuSAV2, y_SuSAV2, color='silver', label='SuSAV2')
    axes[0].set_xlabel(f"$\\nu$")
    axes[0].set_ylabel(f"$\\frac{{d^2\\sigma}}{{d\\Omega \\, d\\nu}}$")
    axes[0].set_title(f"Experiment:{str(dataSet).replace(":", "")} $E_{{0}}$:{E0} $\\Theta$:{ThetaDeg} Z:{Z} A:{A}")
    axes[0].legend()
    axes[1].errorbar(x_valid, R, yerr=Rerr, fmt='o', markersize=4, color='blue', label='Data / Scaled 12C')
    axes[1].errorbar(x_SuSAV2, R_SuSAV2, yerr=Rerr_SuSAV2, fmt='o', markersize=4, color='silver', label='Data / SuSAV2')
    axes[1].axhline(y=1.0, color='black', linestyle='--')  
    axes[1].set_xlabel(f"$\\nu$")
    axes[1].set_ylabel('R')
    axes[1].set_title(f'Scaled 12C ${average:.3f} \\pm {average_error:.3f}$ SuSAV2 ${average_SuSAV2:.3f} \\pm {average_error_SuSAV2:.3f}$')

    # Weighted Average Version
    # axes[1].set_title(f'Ratio (<R> = {weighted_average:.3f} $\\pm$ {weighted_average_error:.3f})')

    axes[1].legend()
    pdf.savefig(fig)
    plt.close(fig)

def write_E0_ThetaDeg(E0, ThetaDeg):
    with open("input1.txt", "w") as input1:
        input1.write(f"{E0} {ThetaDeg}\n")

def write_nu(E0, ThetaDeg, dataSet):
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet) & (df['Ex'] > element_dict[element]["ex_cut_lower"]) & (df['Ex'] < element_dict[element]["ex_cut_upper"]) & (df['nu'] > 0.013)]
    if filtered_data.empty:
        print(f"No data for E0 {E0} and ThetaDeg {ThetaDeg}.")
        return False
    nu_vals = filtered_data['nu'] 
    with open("input2.txt", "w") as input2:
        for nu in nu_vals:
            input2.write(f"{nu}\n")
    return True

if __name__ == "__main__":
    value_pairs = sorted(set((row["E0"], row["ThetaDeg"], row["dataSet"]) for _, row in df.iterrows()), key=lambda x: (x[2], x[0], x[1]))
    for E0, ThetaDeg, dataSet in value_pairs:
        write_E0_ThetaDeg(E0, ThetaDeg)
        if write_nu(E0, ThetaDeg, dataSet):
            with open("output1.txt", "w") as output_file:
                subprocess.run(["./qemodplot"], stdout=output_file)
            # QE and NS shifted
            with open("input2.txt", "r") as file:
                lines = file.readlines()
            with open("input2.txt", "w") as file:
                for line in lines:
                    try:
                        value = float(line.strip())  
                        file.write(f"{value - 0.013}\n")
                    except ValueError:
                        print(f"Skipping invalid line: {line.strip()}")
            with open("output2.txt", "w") as output_file:
                subprocess.run(["./qemodplot"], stdout=output_file)
            #
            plot_cross_section(E0, ThetaDeg, dataSet)
    pdf.close()
