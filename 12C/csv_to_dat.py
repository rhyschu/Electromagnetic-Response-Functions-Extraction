import csv

csv_filename = '56Fe.csv'  
dat_filename = '56Fe.dat'  
with open(csv_filename, 'r', newline='') as csv_file:
    reader = csv.reader(csv_file)
    with open(dat_filename, 'w', newline='') as dat_file:
        for row in reader:
            dat_file.write('\t'.join(row) + '\n')
