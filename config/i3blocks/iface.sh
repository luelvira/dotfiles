#!/usr/bin/env bash

# ips=$(ip a | grep 'wlp6s0\|tun0'  | grep -Eo 'inet[[:space:]]([0-9]{1,3}\.?){4}')
# nips=$( wc -l <<< $ips)
# [ $nips -eq 2 ]  && awk '{printf (NR%2==0) ? $2 : $2" | "}' <<< $ips || awk '{printf $2 }' <<< $ips

ips=$(ip a | grep 'wlp6s0\|tun0'  | grep -Eo 'inet[[:space:]]([0-9]{1,3}\.?){4}')
if [ $(wc -l <<< $ips ) -eq 2 ];then
	connected=1
	awk '{printf (NR%2==0) ? $2 : $2" | "}' <<< $ips
else
	connected=0
	awk '{printf $2 }' <<< $ips
fi

case $BLOCK_BUTTON in
	1) if [ $connected -eq 0 ]; then
			nmcli con up id UC3M_CON_CA passwd-file ~/Documents/openvpn/.passwordfile > /dev/null
		else
			nmcli con down UC3M_CON_CA > /dev/null 
		fi
esac
