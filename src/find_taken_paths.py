# find the universe # corresponding to paths taken by analysts
import pandas as pd

taken = pd.read_csv('analysts.csv', dtype=str, na_filter=False)

df = pd.read_csv('multiverse/summary.csv', dtype=str, na_filter=False)

for idx, row in taken.iterrows():
  res = df.loc[
    (df['Unit'] == row['Unit']) &
    (df['Model'] == row['Model']) &
    (df['filter'] == row['filter']) &
    (df['DV'] == row['DV']) &
    (df['IV'] == row['IV']) &
    (df['covariates'] == row['covariates'])
  ]
  if res.shape[0] > 0:
    print(f'{row.analyst} is {res.Filename.iloc[0]}')
  else:
    print(f'Cannot find {row.analyst}')
