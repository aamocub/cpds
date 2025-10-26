#!/bin/bash

make build >/dev/null
cd .build

# Baseline: 5 clients, 50 entries, 1 read/trans, 1 write/trans, 5 seconds
# note: changed for the subset variation
erl -noshell -eval "timey:start(5, 50, 4, 5)"

BASELINE="Clients;Entries;Rd;Wr;Sec"
# STARTLINE="$BASELINE"
# for client in $(seq 1 50)
# do
#     STARTLINE="$STARTLINE;Client$client"
# done
# 
# ## NUMBER OF CLIENTS (1RD 1WR)
# echo $STARTLINE > ../csv/timey_clients_50_1rd1wr.csv
# for client in $(seq 1 50)
# do
#     erl -noshell -eval "timey:start($client, 50, 1, 1, 5)" >> ../csv/timey_clients_50_1rd1wr.csv
# done
# 
# ## NUMBER OF CLIENTS (2RD 2WR)
# echo $STARTLINE > ../csv/timey_clients_50_2rd2wr.csv
# for client in $(seq 1 50)
# do
#     erl -noshell -eval "timey:start($client, 50, 2, 2, 5)" >> ../csv/timey_clients_50_2rd2wr.csv
# done

## NUMBER OF ENTRIES
STARTLINE="$BASELINE"
for client in $(seq 1 5)
do
    STARTLINE="$STARTLINE;Client$client"
done
# echo $STARTLINE > ../csv/timey_nentries_1rd1wr.csv
# for entries in $(seq 1 50)
# do
#     erl -noshell -eval "timey:start(5, $entries, 1, 1, 5)" >> ../csv/timey_nentries_1rd1wr.csv
# done
# echo $STARTLINE > ../csv/timey_nentries_2rd2wr.csv
# for entries in $(seq 1 50)
# do
#     erl -noshell -eval "timey:start(5, $entries, 2, 2, 5)" >> ../csv/timey_nentries_2rd2wr.csv
# done
# 
# ## NUMBER OF READS
# STARTLINE="$BASELINE"
# for client in $(seq 1 5)
# do
#     STARTLINE="$STARTLINE;Client$client"
# done
# echo $STARTLINE > ../csv/timey_reads.csv
# for rd in $(seq 0 20)
# do
#     erl -noshell -eval "timey:start(5, 50, $rd, 1, 5)" >> ../csv/timey_reads.csv
# done
# 
# ## NUMBER OF WRITES
# echo $STARTLINE > ../csv/timey_writes.csv
# for wr in $(seq 0 20)
# do
#     erl -noshell -eval "timey:start(5, 50, 1, $wr, 5)" >> ../csv/timey_writes.csv
# done

## RATIO READS/WRITES
# echo $STARTLINE > ../csv/timey_ratio10.csv
# for rd in $(seq 0 10)
# do
#     erl -noshell -eval "timey:start(5, 50, $rd, $((10-$rd)), 5)" >> ../csv/timey_ratio10.csv
# done
# 
# echo $STARTLINE > ../csv/timey_ratio20.csv
# for rd in $(seq 0 20)
# do
#     erl -noshell -eval "timey:start(5, 50, $rd, $((20-$rd)), 5)" >> ../csv/timey_ratio20.csv
# done

## NUMBER OF SUBSETS
echo $STARTLINE > ../csv/timey_subsets.csv
for ssize in $(seq 1 30)
do
    erl -noshell -eval "timey:start(5, 50, $ssize, 5)" >> ../csv/timey_subsets.csv
done

# echo 'Clients;Entries;Rd;Wr;Sec;Client1;Client2;Client3;Client4;Client5;Avg;;'

# for entries in $(seq 1 30)
# do
#     erl -noshell -eval "opty:start(5, $entries, 2, 2, 5)"
# done
# erl -noshell -eval "opty:start(5, 50, 2, 2, 5)"

# Reads
# for rd in $(seq 1 20)
# do
#     erl -noshell -eval "opty:start(5, 50, $rd, 1, 5)"
# done

# Writes
# for wr in $(seq 1 20)
# do
#     erl -noshell -eval "opty:start(5, 50, 1, $wr, 5)"
# done

# Read/write ratio
# for rd in $(seq 0 10)
# do
#     erl -noshell -eval "opty:start(5, 50, $rd, $((10-$rd)), 5)"
# done

