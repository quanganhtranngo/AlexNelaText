awesome fun commands to run

### Step 0: bruh moment

Make sure you are in the same directory as this repo within your terminal.

### Step 1: Preprocessing

1. `pip install spacy`
2. run pre.py (press run or `python3 pre.py` in terminal)

### Step 2: GloVe (https://github.com/stanfordnlp/GloVe/tree/master)

1. `git clone https://github.com/stanfordnlp/glove`
2. open demo.sh found in the glove folder
3. change the line `CORPUS=text8` to `CORPUS=../corpora_millennium/1800.txt` (or `1800.txt`)
4. change the line `SAVE_FILE=vectors` to something more specific like `SAVE_FILE=vectors1800` (or `1900`)
5. change `MAX_ITER` as desired
6. run it with `cd glove && make && ./demo.sh && cd ..`

### Step 3: WEAT time (https://github.com/chadaeun/weat_replication/blob/master/lib/weat.py)

1. `git clone https://github.com/chadaeun/weat_replication.git`
2. open weat/weat.json and add one of those files in the same format, if you want
3. `python3 weat_replication/convert_weat.py --weat_dir weath_replication/weat --output weat_replication/weat/weat.json`
4. `python3 weat_replication/weat_test/py --word_embedding_type glove --word_embedding_path glove/vectors1800.txt --weat_path weat_replication/weat/weat.json --output output1800.csv`
