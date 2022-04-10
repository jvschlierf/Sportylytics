import pandas as pd

### WORK IN PROGRESS:

df = pd.read_csv('spotrac_contracts.csv')

counts = pd.read_csv('value_counts.csv')
for value in counts['Unnamed: 0']:
    print(value)