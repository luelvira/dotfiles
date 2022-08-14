#!/usr/bin/env bash

if [ $(bluetoothctl info B0:18:6F:0B:1D:03 | grep Connected | awk '{print $2}') == "yes" ]; then
	bluetoothctl disconnect B0:18:6F:0B:1D:03 
else
	bluetoothctl connect B0:18:6F:0B:1D:03 
fi
pkill -RTMIN+10 i3blocks

