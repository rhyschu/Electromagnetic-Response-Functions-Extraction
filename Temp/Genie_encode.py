import re
import csv

input_file = 'C12_Genie.txt'
output_file = 'C12_Genie.csv'


header = ['Z', 'A', 'E0', 'ThetaDeg', 'nu', 'cross', 'error']
pattern = re.compile(r'^\s*\d+\s+\d+\s+[\d.eE+-]+\s+[\d.eE+-]+\s+[\d.eE+-]+\s+[\d.eE+-]+\s+[\d.eE+-]+\s*$')

with open(input_file, 'r') as infile, open(output_file, 'w', newline='') as outfile:
    writer = csv.writer(outfile)
    writer.writerow(header)

    for line in infile:
        if pattern.match(line):
            values = re.findall(r'[\d.eE+-]+', line)
            if len(values) >= 7:
                writer.writerow(values[:7])
