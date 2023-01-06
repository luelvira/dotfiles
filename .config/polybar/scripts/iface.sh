#!/usr/bin/env bash

# ips=$(ip a | grep 'wlp6s0\|tun0'  | grep -Eo 'inet[[:space:]]([0-9]{1,3}\.?){4}')
# nips=$( wc -l <<< $ips)
# [ $nips -eq 2 ]  && awk '{printf (NR%2==0) ? $2 : $2" | "}' <<< $ips || awk '{printf $2 }' <<< $ips

ips=$(ip a | grep 'wlp6s0\|tun0'  | grep -Eo 'inet[[:space:]]([0-9]{1,3}\.?){4}')
if [ -z "$ips" ]; then
	echo -e "%{U#cb5760} :("
elif [ $(wc -l <<< $ips ) -eq 2 ]; then
	connected=1
	ips_format=$(awk '{printf (NR%2==0) ? $2 : $2" | "}' <<< $ips)
	echo -e "%{F#999f63}%{U#999f63}$ips_format"
else
	connected=0
	ip=$(awk '{printf $2}' <<< $ips)
	echo -e "%{U#999f63}$ip"
fi

case $1 in
	1) if [ $connected -eq 0 ]; then
			nmcli con up id UC3M_CON_CA_PAS  > /dev/null
		else
			nmcli con down UC3M_CON_CA_PAS > /dev/null 
		fi
esac
