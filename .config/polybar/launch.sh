#!/bin/bash

pkill polybar
#polybar -r main &

for m in $(polybar --list-monitors | cut -d":" -f1); do
	if [ "$m" == "DP-1" ];then 
    	MONITOR=$m polybar --reload main &
	else
    	MONITOR=$m polybar --reload main2 &
	fi
done
