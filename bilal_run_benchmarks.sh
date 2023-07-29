#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

for ((i=8; i>=1; i--))
do
    echo "> send_message_bilal, $i threads"
    erl +S $i -noshell -s benchmarkBilal test_send_message_bilal -s init stop > "benchmarks/result-send_message-$i.txt"
done
”