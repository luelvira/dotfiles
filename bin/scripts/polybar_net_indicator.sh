#!/bin/bash
#
# Show a custom network connection indicator in polybar.
# Includes code for wifi, VPN, ZeroTier, and Hamachi.
#
# Author: machaerus
# https://gitlab.com/machaerus

source colors.sh

net_print() {

	# CONNECTED_WIFI=$(iwconfig wlan0 | grep ESSID | wc -l)
	ESSID=$(nmcli d | grep  " connected" | awk '{print $4}')
	echo $ESSID
	
}

net_print


