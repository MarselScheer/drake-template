#!/bin/bash

timestamp=$(date +"%Y-%m-%d_%H_%M")
mv make_*log ./log/
nohup bash -c "Rscript make.R" > make_$timestamp.log &

echo "Script started. You can log out. Start monitor INFO in 5 seconds"
sleep 5
tail -F make_*log | grep 'INFO\|target'

