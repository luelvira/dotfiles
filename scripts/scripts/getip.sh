#!/usr/bin/env bash
ip="$(ip a s wlp6s0 | grep -o -P '(?<=inet )[0-9]{1,3}(\.[0-9]{1,3}){3}')"
ip_vpn="$(ip l | grep tun0)"

if [[ -n $ip_vpn ]]; then
	ip_vpn="$(ip a s tun0 | grep -o -P '(?<=inet )[0-9]{1,3}(\.[0-9]{1,3}){3}')"
else
	ip_vpn=""
fi

if [ "$ip_vpn" != "" ]; then
	printf "$ip_vpn>"
fi

if [ "$ip" != "" ]; then
	printf $ip
else
	printf "\0x0"
fi

