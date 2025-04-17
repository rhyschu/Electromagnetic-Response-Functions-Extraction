import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import subprocess
from matplotlib.backends.backend_pdf import PdfPages

data = '2H.csv'
element = '2H'
element_dict = {
    "1H": {"A": 1, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1/6},
    "2H": {"A": 2, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1/6},
    "12C": {"A": 12, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1},
    "40Ca": {"A": 40, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 20/6},
    "56Fe": {"A": 56, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 26/6}
}
mass_nucleus = element_dict[element]["A"]*0.931494
pdf = PdfPages("H2_Cross_Section.pdf")
df = pd.read_csv(data)
df["ThetaRad"] = df["ThetaDeg"]*np.pi/180
df["sin2(T/2)"] = (np.sin(df["ThetaRad"]/2))**2
df["nuel"] = df["E0"] - df["E0"]/(1 + 2*df["E0"]*df["sin2(T/2)"]/mass_nucleus)
df["Ex"] = df["nu"] - df["nuel"]

def plot_cross_section(E0, ThetaDeg, dataSet):
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet) & (df['Ex'] > element_dict[element]["ex_cut_lower"]) & (df['Ex'] < element_dict[element]["ex_cut_upper"]) & (df['nu'] > 0.013)]
    Z = filtered_data['Z'].iloc[0]
    A = filtered_data['A'].iloc[0]
    x = filtered_data['nu']  
    y = filtered_data['cross']  
    yerr = filtered_data['error']  
    x_fit = []
    y_fit = []
    with open('output1.txt', 'r') as file1:
        for line1 in file1:
            columns1 = line1.split()      
            x_fit.append(float(columns1[2]))  
            y_fit.append((float(columns1[6]))*1000*12*element_dict[element]["multiplier"])
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

    fig, axes = plt.subplots(1, 2, figsize=(10, 6))
    axes[0].errorbar(x, y, yerr=yerr, fmt='o', markersize=4, color='red', label='Data')
    axes[0].scatter(x_fit, y_fit, s=6, color='blue')
    axes[0].plot(x_fit, y_fit, color='blue', label='Scaled 12C')
    axes[0].set_xlabel(f"$\\nu$")
    axes[0].set_ylabel(f"$\\frac{{d^2\\sigma}}{{d\\Omega \\, d\\nu}}$")
    axes[0].set_title(f"Experiment:{str(dataSet).replace(":", "")} $E_{{0}}$:{E0} $\\Theta$:{ThetaDeg} Z:{Z} A:{A}")
    axes[0].legend()
    axes[1].errorbar(x_valid, R, yerr=Rerr, fmt='o', markersize=4, color='blue', label='Data / Scaled 12C')
    axes[1].axhline(y=1.0, color='black', linestyle='--')  
    axes[1].set_xlabel(f"$\\nu$")
    axes[1].set_ylabel('R')
    axes[1].set_title(f'Scaled 12C ${average:.3f} \\pm {average_error:.3f}$')

    # Weighted Average Version
    # axes[1].set_title(f'Ratio (<R> = {weighted_average:.3f} $\\pm$ {weighted_average_error:.3f})')

    axes[1].legend()
    pdf.savefig(fig)
    plt.close(fig)

def write_E0_ThetaDeg(E0, ThetaDeg):
    with open("input1.txt", "w") as input1:
        input1.write(f"{E0} {ThetaDeg}\n")

def write_nu(E0, ThetaDeg, dataSet):
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['dataSet'] == dataSet) & (df['Ex'] > element_dict[element]["ex_cut_lower"]) & (df['Ex'] < element_dict[element]["ex_cut_upper"])]
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
            plot_cross_section(E0, ThetaDeg, dataSet)
    pdf.close()
