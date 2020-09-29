# find the universe # corresponding to paths taken by analysts
import pandas as pd

def find_taken_paths (fn='multiverse/summary.csv'):
  taken = pd.read_csv('analysts.csv', dtype=str, na_filter=False)

  df = pd.read_csv(fn, dtype=str, na_filter=False)

  found = []
  for idx, row in taken.iterrows():
    res = df.loc[
      (df['Unit'] == row['Unit']) &
      (df['Model'] == row['Model']) &
      (df['filter'] == row['filter']) &
      (df['DV'] == row['DV']) &
      (df['IV'] == row['IV']) &
      (df['covariates'] == row['covariates']) &
      (df['random_term'] == row['random_term'])
    ]
    uid = res.Filename.iloc[0] if res.shape[0] > 0 else ''
    found.append({'analyst': row.analyst, 'uid': uid})

  return found


if __name__ == '__main__':
  res = find_taken_paths()
  for r in res:
    print('{} is {}'.format(r['analyst'], r['uid']))
