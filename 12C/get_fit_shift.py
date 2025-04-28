import pandas as pd
import subprocess

data = 'Fe56.csv'
data_fit = 'Fe56_Fit.csv'
elem= '56Fe'
elem_dict = {
    "1H": {"A": 1, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1/6, "QENS_shift": 0, "NS_factor": 1},
    "2H": {"A": 2, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1/6, "QENS_shift": 0, "NS_factor": 1},
    "12C": {"A": 12, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 1, "QENS_shift": 0, "NS_factor": 1},
    "40Ca": {"A": 40, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 20/6, "QENS_shift": 0, "NS_factor": 1},
    "56Fe": {"A": 56, "ex_cut_lower": 0, "ex_cut_upper": 1000, "multiplier": 26/6, "QENS_shift": 0.013, "NS_factor": 0.5}
}

if __name__ == "__main__":
    df = pd.read_csv(data)
    with open(data_fit, "w") as response_file:
        response_file.write("Data:Z,A,E0,ThetaDeg,nu,cross,error,dataSet,Fit:ep,theta,nu,Ex,W2,q2,sigtot,sigqe,sigie,sigmec,nuccstot,signonuc,sigtot_shifted,qe_shifted,mec_shifted,ns_shifted,ratio\n")
    for index, row in df.iterrows():
        Z = row['Z']
        A = row['A']
        E0 = row['E0']
        ThetaDeg = row['ThetaDeg']
        nu = row['nu']
        cross = row['cross']
        error = row['error']
        dataSet = row['dataSet']
        if nu <= elem_dict[elem]["QENS_shift"]:
            continue
        with open("input1.txt", "w") as input1:
            input1.write(f"{E0} {ThetaDeg}\n")
        with open("input2.txt", "w") as input2:
            input2.write(f"{nu}\n")
        with open("output1.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        with open("input2.txt", "w") as input2:
            input2.write(f"{nu - elem_dict[elem]["QENS_shift"]}\n")
        with open("output2.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        with open("output1.txt", "r") as output_file:
            line = output_file.readline().strip()
            values = line.split()
        with open("output2.txt", "r") as output_file:
            line = output_file.readline().strip()
            values_shifted = line.split()
        for i in range(6, 12):
            values[i] = float(values[i]) * 1000 * 12 * elem_dict[elem]["multiplier"]
            values_shifted[i] = float(values_shifted[i]) * 1000 * 12 * elem_dict[elem]["multiplier"]
        ep = values[0]
        theta = values[1]
        nu_fit = values[2]
        Ex = float(values[3])
        W2 = values[4]
        q2 = values[5]
        sigtot = values[6]
        sigqe = values[7]
        sigie = values[8]
        sigmec = values[9]
        nuccstot = values[10]
        signonuc = values[11]
        sigtot_shifted = values[6] - values[7] + values_shifted[7] - values[9] + values_shifted[9] - values[10] + values_shifted[10] * elem_dict[elem]["NS_factor"]
        qe_shifted = values_shifted[7]
        mec_shifted = values_shifted[9]
        ns_shifted = values_shifted[10] * elem_dict[elem]["NS_factor"]
        if sigtot_shifted == 0 or Ex < elem_dict[elem]["ex_cut_lower"] or Ex > elem_dict[elem]["ex_cut_upper"]:
            continue
        ratio = float(cross) / sigtot_shifted
        with open(data_fit, "a") as response_file:
            response_file.write(f"{Z},{A},{E0},{ThetaDeg},{nu},{cross},{error},{dataSet},{ep},{theta},{nu_fit},{Ex},{W2},{q2},{sigtot},{sigqe},{sigie},{sigmec},{nuccstot},{signonuc},{sigtot_shifted},{qe_shifted},{mec_shifted},{ns_shifted},{ratio}\n")
