#!/bin/bash

FREE=`free -m | awk 'NR == 3 {gsub(/%/,""); print $3}'`
PERC=`echo $FREE*100/2012 | bc`
PERCBAR=`echo -e "$PERC" | gdbar -bg '#454545' -fg '#cd546c' -h 1 -w 50 | sed "s/\ .*\%//g"`

echo "$PERCBAR"
