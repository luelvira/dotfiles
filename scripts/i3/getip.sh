#!/usr/bin/env bash
ip="$(ip a s wlp6s0 | grep -o -P '(?<=inet )[0-9]{1,3}(\.[0-9]{1,3}){3}')"
ip_vpn="$(ip a s tun0 | grep -o -P '(?<=inet )[0-9]{1,3}(\.[0-9]{1,3}){3}')"

if [ "$ip_vpn" != "" ]; then
	printf "<icon>network-vpn-symbolic</icon>"
	printf "<txt>${ip_vpn}</txt>"
elif [ "$ip" != "" ]; then
	printf "<icon>network-vpn-disconnected-symbolic</icon>"
else
	printf "\0x0"
fi
printf "<tool>VPN IP</tool>"
