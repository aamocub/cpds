#!/bin/bash

make build >/dev/null
cd .build

# Baseline: 5 clients, 50 entries, 1 read/trans, 1 write/trans, 5 seconds
# erl -noshell -eval "opty:start(5, 50, 1, 1, 5)"

# for client in $(seq 1 50)
# do
#     erl -noshell -eval "opty:start($client, 50, 2, 2, 5)"
# done

echo 'Clients;Entries;Rd;Wr;Sec;Client1;Client2;Client3;Client4;Client5;Avg;;'

for entries in $(seq 1 30)
do
    erl -noshell -eval "opty:start(5, $entries, 2, 2, 5)"
done
erl -noshell -eval "opty:start(5, 50, 2, 2, 5)"

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

