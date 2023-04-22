import pandas as pd
import os

df = pd.read_csv(f"{os.getcwd()}/data/all.csv")

grouped = df.groupby("Character")

for name, group in grouped:
    filename = f"{os.getcwd()}/data/characters/{name}.csv"
    group.iloc[:, 1:].to_csv(filename, index=False)
