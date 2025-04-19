import pandas as pd
import os
import csv

if __name__ == "__main__":

    df = pd.read_csv('weighted_average.csv')
    sorted_df = df.sort_values(by=['dataSet', 'E0', 'ThetaDeg'], ascending=[True, True, True])
    sorted_df.to_csv('weighted_average.csv', index=False)

    df = pd.read_csv('weighted_average.csv')
    dataSet_E0_pairs = list(set((row["dataSet"], row["E0"], row["Z"], row["A"]) for _, row in df.iterrows()))
    for dataSet, E0, Z, A in dataSet_E0_pairs:
        filtered_data = df[(df['dataSet'] == dataSet) & (df['E0'] == E0)]
        value = filtered_data['weighted_average']
        error = filtered_data['weighted_average_error']
        value = value.reset_index(drop=True)
        error = error.reset_index(drop=True)
        sum1 = 0
        sum2 = 0
        for i in range(len(value)):
            sum1 += value[i] / (error[i] ** 2)
            sum2 += 1 / (error[i] ** 2)
        dataSet_E0_weighted_average = sum1 / sum2
        dataSet_E0_weighted_average_error = (1 / sum2) ** 0.5
        file_exists = os.path.isfile("weighted_average_energy.csv")
        with open("weighted_average_energy.csv", mode="a", newline="") as file:
            writer = csv.writer(file)
            if not file_exists:
                writer.writerow(["dataSet", "E0", "Z", "A", "weighted_average", "weighted_average_error"])
            writer.writerow([dataSet, E0, Z, A, dataSet_E0_weighted_average, dataSet_E0_weighted_average_error])
    df = pd.read_csv('weighted_average_energy.csv')
    sorted_df = df.sort_values(by=['dataSet', 'E0'], ascending=[True, True])
    sorted_df.to_csv('weighted_average_energy.csv', index=False)

    df = pd.read_csv('weighted_average.csv')
    dataSet_ThetaDeg_pairs = list(set((row["dataSet"], row["ThetaDeg"], row["Z"], row["A"]) for _, row in df.iterrows()))
    for dataSet, ThetaDeg, Z, A in dataSet_ThetaDeg_pairs:
        filtered_data = df[(df['dataSet'] == dataSet) & (df['ThetaDeg'] == ThetaDeg)]
        value = filtered_data['weighted_average']
        error = filtered_data['weighted_average_error']
        value = value.reset_index(drop=True)
        error = error.reset_index(drop=True)
        sum1 = 0
        sum2 = 0
        for i in range(len(value)):
            sum1 += value[i] / (error[i] ** 2)
            sum2 += 1 / (error[i] ** 2)
        dataSet_ThetaDeg_weighted_average = sum1 / sum2
        dataSet_ThetaDeg_weighted_average_error = (1 / sum2) ** 0.5
        file_exists = os.path.isfile("weighted_average_angle.csv")
        with open("weighted_average_angle.csv", mode="a", newline="") as file:
            writer = csv.writer(file)
            if not file_exists:
                writer.writerow(["dataSet", "ThetaDeg", "Z", "A", "weighted_average", "weighted_average_error"])
            writer.writerow([dataSet, ThetaDeg, Z, A, dataSet_ThetaDeg_weighted_average, dataSet_ThetaDeg_weighted_average_error])
    df = pd.read_csv('weighted_average_angle.csv')
    sorted_df = df.sort_values(by=['dataSet', 'ThetaDeg'], ascending=[True, True])
    sorted_df.to_csv('weighted_average_angle.csv', index=False)

    df = pd.read_csv('weighted_average.csv')
    dataSet_pairs = list(set((row["dataSet"], row["Z"], row["A"]) for _, row in df.iterrows()))
    for dataSet, Z, A in dataSet_pairs:
        filtered_data = df[(df['dataSet'] == dataSet)]
        value = filtered_data['weighted_average']
        error = filtered_data['weighted_average_error']
        value = value.reset_index(drop=True)
        error = error.reset_index(drop=True)
        sum1 = 0
        sum2 = 0
        for i in range(len(value)):
            sum1 += value[i] / (error[i] ** 2)
            sum2 += 1 / (error[i] ** 2)
        dataSet_weighted_average = sum1 / sum2
        dataSet_weighted_average_error = (1 / sum2) ** 0.5
        file_exists = os.path.isfile("weighted_average_dataset.csv")
        with open("weighted_average_dataset.csv", mode="a", newline="") as file:
            writer = csv.writer(file)
            if not file_exists:
                writer.writerow(["dataSet", "Z", "A", "weighted_average", "weighted_average_error"])
            writer.writerow([dataSet, Z, A, dataSet_weighted_average, dataSet_weighted_average_error])
    df = pd.read_csv('weighted_average_dataset.csv')
    sorted_df = df.sort_values(by=['dataSet'], ascending=[True])
    sorted_df.to_csv('weighted_average_dataset.csv', index=False)
