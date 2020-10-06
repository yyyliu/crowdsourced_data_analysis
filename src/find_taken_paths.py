# find the universe # corresponding to paths taken by analysts
import pandas as pd
import click
import os
import re


def find_taken_paths (fn='multiverse/summary.csv'):
  """ find the universes correponding to the analyst's scripts """
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


def fn_to_uid (fn):
  """ extract the integer UID from the filename string """
  rg = re.compile('^universe_(\\d+)\\.R')
  m = re.match(rg, fn)
  if m:
    return int(m.group(1))
  return -1


@click.command()
@click.option('--summary', default='multiverse/summary.csv', help='Path to summary.csv')
@click.option('--save', default='', help='CSV file to save this as a column')
def main (summary, save):
  res = find_taken_paths(summary)

  if save:
    # check path
    if not os.path.exists(save):
      print(f'Cannot find the file {save}')
      return

    # wrangle the taken paths
    taken = pd.DataFrame(res)
    taken['uid'] = taken['uid'].map(fn_to_uid)

    # read and join
    df = pd.read_csv(save, dtype=str, na_filter=False)
    df['uid'] = df['uid'].astype(int)
    df = pd.merge(df, taken, how='left', on='uid')
    # print(df.loc[df['analyst'].notna()])

    # save
    df.to_csv(save)

  else:
    # print
    for r in res:
      print('{} is {}'.format(r['analyst'], r['uid']))


if __name__ == '__main__':
  main()
