import csv

def csv_to_dat(csv_filename, dat_filename, delimiter='\t'):

    with open(csv_filename, 'r', newline='') as csv_file:
        reader = csv.reader(csv_file)
        with open(dat_filename, 'w', newline='') as dat_file:
            for row in reader:
                dat_file.write(delimiter.join(row) + '\n')

if __name__ == "__main__":
    csv_filename = '56Fe.csv'  
    dat_filename = '56Fe.dat'  
    csv_to_dat(csv_filename, dat_filename)
