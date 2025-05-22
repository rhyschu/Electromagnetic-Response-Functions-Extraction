import pandas as pd
import subprocess

data = 'Data/Fe56_Fake_raw.csv'
data_fit = 'Data/Fe56_Fake.csv'
elem= 'Fe56'
ex_cut_lower = 0
ex_cut_upper = 1000
multiplier = 26 / 6
QE_shift = 0.0

if __name__ == "__main__":
    df = pd.read_csv(data)
    with open(data_fit, "w") as response_file:
        response_file.write("Data:Z,A,E0,ThetaDeg,nu,cross,error,dataSet,Fit:ep,theta,nu_fit,Ex,W2,q2,sigtot,sigqe,sigie,sigmec,nuccstot,signonuc,sigtot_shifted,qe_shifted,ratio\n")
    for index, row in df.iterrows():
        Z = row['Z']
        A = row['A']
        E0 = row['E0']
        ThetaDeg = row['ThetaDeg']
        nu = row['nu']
        cross = row['cross']
        error = row['error']
        dataSet = row['dataSet']
        if nu <= QE_shift:
            continue
        with open("input1.txt", "w") as input1:
            input1.write(f"{E0} {ThetaDeg}\n")
        with open("input2.txt", "w") as input2:
            input2.write(f"{nu}\n")
        with open("output1.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        with open("input2.txt", "w") as input2:
            input2.write(f"{nu - QE_shift}\n")
        with open("output2.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        with open("output1.txt", "r") as output_file:
            line = output_file.readline().strip()
            values = line.split()
        with open("output2.txt", "r") as output_file:
            line = output_file.readline().strip()
            values_shifted = line.split()
        for i in range(6, 12):
            values[i] = float(values[i]) * 1000 * 12 * multiplier
            values_shifted[i] = float(values_shifted[i]) * 1000 * 12 * multiplier
        ep = values[0]
        theta = values[1]
        nu_fit = values[2]
        Ex = float(values[3])
        if Ex < ex_cut_lower or Ex > ex_cut_upper:
            continue
        W2 = values[4]
        q2 = values[5]
        sigtot = values[6]
        sigqe = values[7]
        sigie = values[8]
        sigmec = values[9]
        nuccstot = values[10]
        signonuc = values[11]
        sigtot_shifted = values[6] - values[7] + values_shifted[7] - values[9] + values_shifted[9]
        qe_shifted = values_shifted[7] + values_shifted[9]
        if sigtot_shifted == 0:
            ratio = 0
        else:
            ratio = float(cross) / sigtot_shifted
        with open(data_fit, "a") as response_file:
            response_file.write(f"{Z},{A},{E0},{ThetaDeg},{nu},{cross},{error},{dataSet},{ep},{theta},{nu_fit},{Ex},{W2},{q2},{sigtot},{sigqe},{sigie},{sigmec},{nuccstot},{signonuc},{sigtot_shifted},{qe_shifted},{ratio}\n")
