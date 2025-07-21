import pandas as pd
from itertools import permutations
import numpy as np

def yield_permutations(df, col = 'h'):
    grouped = df.groupby(col)
    for name, group in grouped:
        for i,j in permutations(group['i'].values,2):
            yield (i,j)

def get_identity(n, as_df = True):
    rows = list()
    cols = list()
    for i in range(n):
        rows.append(i)
        cols.append(i)
    rows = np.array(rows)
    cols = np.array(cols)
    if as_df:
        df = pd.DataFrame({'i':rows,'j':cols}, dtype = "uint32")
        return df
    else:
        return (rows,cols)

def get_permutations(df, as_df = True):
    rows = list()
    cols = list()
    for i,j in yield_permutations(df):
        rows.append(i)
        cols.append(j)
    rows = np.array(rows)
    cols = np.array(cols)
    if as_df:
        df = pd.DataFrame({'i':rows,'j':cols}, dtype = "uint32")
        df = df.drop_duplicates().reset_index(drop= True)
        return df
    else:
        return (rows,cols)