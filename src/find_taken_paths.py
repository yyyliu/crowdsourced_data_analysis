# find the universe # corresponding to paths taken by analysts
import pandas as pd
import click
import os
import re
import subprocess
import math


def find_taken_paths (fn='multiverse'):
  """ find the universes correponding to the analyst's scripts """
  fna = os.path.join(fn, '..', 'analysts.csv')
  taken = pd.read_csv(fna, dtype=str, na_filter=False)

  fn = os.path.join(fn, 'summary.csv')
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


def compare_outputs (actual, expected):
  """ helper function for testing """
  res = ''

  for col in ['z', 'p.value']:
    ve = expected[col].iloc[0]
    va = actual[col][0]
    if not math.isclose(ve, va, rel_tol=1e-4):
      res += f'Expected {col}={ve}, actual {col}={va}\n'

  return res


def run_test (analysts, base='multiverse'):
  """ run the scripts and compare outputs to expected values """
  fn = os.path.join(base, '..', 'analysts.csv')
  taken = pd.read_csv(fn)
  passed = []
  failed = []

  p = os.path.join(base, 'results')
  if not os.path.exists(p):
    os.mkdir(p)

  for a in analysts:
    # if file not found, skip
    if not a['uid']:
      continue

    team = a['analyst']
    print(f'Running {team}')

    # run the corresponding script
    args = ['Rscript', a['uid']]
    p = os.path.join(base, 'code')
    subprocess.run(args, cwd=p, capture_output=True)

    # read output
    uid = fn_to_uid(a['uid'])
    p = os.path.join(base, 'results', f'estimate_{uid}.csv')
    actual = pd.read_csv(p)

    # compare with expected
    err = compare_outputs(actual, taken.loc[taken['analyst'] == team])
    if err:
      failed.append({'analyst': team, 'error': err})
    else:
      passed.append(team)

  # print test summary
  print(f'Done.\n\n{len(passed)} passed:\n{", ".join(passed)}')
  print('\nThe following tests failed:\n')
  for f in failed:
    print(f['analyst'], f['error'])


@click.command()
@click.option('--dir', '_dir', default='multiverse', help='Path to the multiverse folder')
@click.option('--save', default='', help='CSV file (relative to dir) to save this as a column')
@click.option('--test', is_flag=True, help='Run and verify outputs')
def main (_dir, save, test):
  # check path
  if not os.path.exists(_dir):
    print(f'Cannot find the directory: {_dir}')
    return

  res = find_taken_paths(_dir)

  if save:
    # check path
    save = os.path.join(_dir, save)
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

  elif test:
    run_test(res, _dir)

  else:
    # print
    for r in res:
      print('{} is {}'.format(r['analyst'], r['uid']))


if __name__ == '__main__':
  main()
