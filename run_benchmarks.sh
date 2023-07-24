#!/usr/bin/env bash

# set -e makes the script exit if any command fails
# set -u makes the script exit if any unset variable is used
# set -o pipefail makes the script exit if any command in a pipeline fails
set -euo pipefail

for i in {1..64}
do
    echo "---"
    echo "> fib, $i threads"
    erl +S $i -noshell -s benchmark test_fib -s init stop > "benchmarks/result-fib-$i.txt"
    echo "---"
    echo "> timeline, $i threads"
    erl +S $i -noshell -s benchmark test_timeline -s init stop > "benchmarks/result-timeline-$i.txt"
    echo "---"
    echo "> send_message, $i threads"
    erl +S $i -noshell -s benchmark test_send_message -s init stop > "benchmarks/result-send_message-$i.txt"
done
