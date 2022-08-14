#!/usr/bin/env bash

uuid=$(bluetoothctl devices | cut -f2 -d' ')
state=$(bluetoothctl info $uuid | grep Connected | cut -f2 -d' ')
if [ "$state" == "yes" ]; then
	bluetoothctl disconnect $uuid
else
	bluetoothctl connect $uuid
fi

