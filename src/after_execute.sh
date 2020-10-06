boba merge estimate_{}.csv -b results --out estimate.csv

cd ..
python find_taken_paths.py --save multiverse/estimate.csv
