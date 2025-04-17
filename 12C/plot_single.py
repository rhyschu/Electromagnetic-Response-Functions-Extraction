import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import subprocess
import os

data = '12C.dat'
element = '12C'
element_dict = {
    "12C": {"mp": 11.178, "cut_lower": 0, "cut_upper": 1000, "multiplier": 1},
    "40Ca": {"mp": 37.225, "cut_lower": 0, "cut_upper": 1000, "multiplier": 20/6},
    "56Fe": {"mp": 55.935, "cut_lower": 0, "cut_upper": 1000, "multiplier": 26/6}
}

def plot_cross_section(E0, ThetaDeg):
    df = pd.read_csv(data, delim_whitespace=True)
    df["ThetaRad"] = df["ThetaDeg"]*np.pi/180
    df["sin2(T/2)"] = (np.sin(df["ThetaRad"]/2))**2
    df["nu_elastic"] = df["E0"] - df["E0"]/(1 + 2*df["E0"]*df["sin2(T/2)"]/element_dict[element]["mp"])
    df["Ex"] = df["nu"] - df["nu_elastic"]
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['Ex'] > element_dict[element]["cut_lower"]) & (df['Ex'] < element_dict[element]["cut_upper"])]
    Z = filtered_data['Z'].iloc[0]
    A = filtered_data['A'].iloc[0]
    dataSet = str(filtered_data['dataSet'].iloc[0]).replace(":", "")
    x = filtered_data['nu']  
    y = filtered_data['cross']  
    yerr = filtered_data['error']  
    x_fit = []
    y_fit = []
    with open('output1.txt', 'r') as file:
        for line in file:
            columns = line.split()
            x_fit.append(float(columns[2])) 
            y_fit.append(float(columns[6])*1000*12*element_dict[element]["multiplier"])    
    if len(x) != len(x_fit):
        print("nu and nu_fit have different dimensions.")
        return
    R = (y / y_fit)
    Rerr = (yerr / y_fit)
    valid_indices = np.isfinite(R) & np.isfinite(Rerr) & (Rerr > 0)
    x_valid = x[valid_indices].reset_index(drop=True)
    R = R[valid_indices].reset_index(drop=True)
    Rerr = Rerr[valid_indices].reset_index(drop=True)
    sum1 = 0
    sum2 = 0
    for i in range(len(R)):
        sum1 += R[i] / (Rerr[i] ** 2)
        sum2 += 1 / (Rerr[i] ** 2)
    weighted_average = sum1 / sum2
    weighted_average_error = (1 / sum2) ** 0.5
    fig, axes = plt.subplots(1, 2, figsize=(10, 6))
    axes[0].errorbar(x, y, yerr=yerr, fmt='o', markersize=4, color='red', label='Data')
    axes[0].scatter(x_fit, y_fit, s=5, color='blue', label='Fit')
    axes[0].set_xlabel(f"$\\nu$")
    axes[0].set_ylabel(f"$\\sigma$")
    axes[0].set_title(f"Experiment:{dataSet} $E_{{0}}$:{E0} $\\Theta$:{ThetaDeg} Z:{Z} A:{A}")
    axes[0].legend()
    axes[1].errorbar(x_valid, R, yerr=Rerr, fmt='o', markersize=4, color='red', label='Data / Fit')
    axes[1].axhline(y=1.0, color='black', linestyle='--')  
    axes[1].set_xlabel(f"$\\nu$")
    axes[1].set_ylabel('R')
    axes[1].set_title(f'R (<R> = {weighted_average:.3f} $\\pm$ {weighted_average_error:.3f})')
    axes[1].legend()
    plt.show()
    os.makedirs("plots", exist_ok=True)
    fig.savefig(f"plots/{dataSet}_E0_{E0}_ThetaDeg_{ThetaDeg}_Z_{Z}_A_{A}.png")
    plt.close(fig)

def write_E0_ThetaDeg(E0, ThetaDeg):
    with open("input1.txt", "w") as input1:
        input1.write(f"{E0} {ThetaDeg}\n")

def write_nu(E0, ThetaDeg):
    df = pd.read_csv(data, delim_whitespace=True)
    df["ThetaRad"] = df["ThetaDeg"]*np.pi/180
    df["sin2(T/2)"] = (np.sin(df["ThetaRad"]/2))**2
    df["nu_elastic"] = df["E0"] - df["E0"]/(1 + 2*df["E0"]*df["sin2(T/2)"]/element_dict[element]["mp"])
    df["Ex"] = df["nu"] - df["nu_elastic"]
    filtered_data = df[(df['E0'] == E0) & (df['ThetaDeg'] == ThetaDeg) & (df['Ex'] > element_dict[element]["cut_lower"]) & (df['Ex'] < element_dict[element]["cut_upper"])]
    if filtered_data.empty:
        print(f"No data for E0 {E0} and ThetaDeg {ThetaDeg}.")
        return False
    nu_vals = filtered_data['nu'] 
    with open("input2.txt", "w") as input2:
        for nu in nu_vals:
            input2.write(f"{nu}\n")
    return True

if __name__ == "__main__":
    E0 = float(input("Enter the value of E0: "))
    ThetaDeg = float(input("Enter the value of ThetaDeg: "))
    write_E0_ThetaDeg(E0, ThetaDeg)
    if write_nu(E0, ThetaDeg):
        with open("output1.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        plot_cross_section(E0, ThetaDeg)
