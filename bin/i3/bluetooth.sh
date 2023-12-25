#!/usr/bin/env bash
# Description: Toggle bluetooth connection

devices_paired=$(bluetoothctl devices | cut -d ' ' -f 2)
if bluetoothctl info $devices_paired | grep -e "^.Connected: no" > /dev/null; then
     bluetoothctl connect $devices_paired
else
     bluetoothctl disconnect $devices_paired
fi
