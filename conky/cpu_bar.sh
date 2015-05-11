#!/bin/bash

LOADAVG=`cat /proc/loadavg | cut -c 1-4`
PERC=`echo $LOADAVG*100 | bc`
PERCBAR=`echo -e "$PERC" | gdbar -bg '#454545' -fg '#cd546c' -h 1 -w 50 | sed "s/\ .*\%//g"`

echo "$PERCBAR"
