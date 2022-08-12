import csv
import os
from email import header

print(os.getcwd())

file = open('DKSalaries.csv')
type(file)

csvreader = csv.reader(file)

header = []
header = next(csvreader)
header

rows = []
for row in csvreader:
    rows.append(row)
rows

file.close()
