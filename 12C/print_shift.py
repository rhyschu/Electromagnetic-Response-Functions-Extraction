import pandas as pd
import subprocess

data = '56Fe.csv'
element = '56Fe'
element_dict = {
    "12C": {"cut_lower": 0, "cut_upper": 1000, "multiplier": 1},
    "40Ca": {"cut_lower": 0, "cut_upper": 1000, "multiplier": 20/6},
    "56Fe": {"cut_lower": 0, "cut_upper": 1000, "multiplier": 26/6}
}

if __name__ == "__main__":
    df = pd.read_csv(data)
    with open("response.csv", "w") as response_file:
        response_file.write("Data:Z,A,E0,ThetaDeg,nu,cross,error,dataSet,Fit:ep,theta,nu,nu-nuel,w2,q2,sigtot,sigqe,sigie,sigmec,nuccstot,signonuc,sigtot_shifted,qe_shifted,ns_shifted,ratio\n")
    for index, row in df.iterrows():
        Z = row['Z']
        A = row['A']
        E0 = row['E0']
        ThetaDeg = row['ThetaDeg']
        nu = row['nu']   
        cross = row['cross']
        error = row['error']
        dataSet = row['dataSet']
        if nu <= 0.013:
            continue
        with open("input1.txt", "w") as input1:
            input1.write(f"{E0} {ThetaDeg}\n")
        with open("input2.txt", "w") as input2:
            input2.write(f"{nu}\n")
        with open("output1.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        with open("input2.txt", "w") as input2:
            input2.write(f"{nu - 0.013}\n")
        with open("output2.txt", "w") as output_file:
            subprocess.run(["./qemodplot"], stdout=output_file)
        with open("output1.txt", "r") as output_file:
            line = output_file.readline().strip()
            values = line.split()
        with open("output2.txt", "r") as output_file:
            line = output_file.readline().strip()
            values_shifted = line.split()
        for i in range(6, 12):
            values[i] = float(values[i])*1000*12*element_dict[element]["multiplier"]
            values_shifted[i] = float(values_shifted[i])*1000*12*element_dict[element]["multiplier"]
        sigtot_shifted = values[6] - values[7] + values_shifted[7] - values[10] + values_shifted[10] / 2
        qe_shifted = values_shifted[7]
        ns_shifted = values_shifted[10] / 2
        values.append(sigtot_shifted)
        values.append(qe_shifted)
        values.append(ns_shifted)
        if sigtot_shifted != 0:
            values.append(float(cross)/sigtot_shifted)
        else:
            values.append('0')
        with open("response.csv", "a") as response_file:
            response_file.write(f"{Z},{A},{E0},{ThetaDeg},{nu},{cross},{error},{dataSet}," + ",".join(map(str, values)) + "\n")
