#!/usr/bin/env bash

# if [ $(bluetoothctl info B0:18:6F:0B:1D:03 | grep Connected | awk '{print $2}') == "yes" ]; then
#    	echo "" 
# else
# 	echo ""
# fi


powered=$(bluetoothctl show | grep "Powered: " | awk '{print $2}')
connected=$(bluetoothctl info B0:18:6F:0B:1D:03 | grep Connected | awk '{print $2}')
echo $LABEL
echo $LABEL
if [ $powered == "yes" ];then
	if [ $connected == "yes" ]; then
		conn=1
		echo \#66ffffff
	else
		echo \#2193ffff
	fi
fi

case $BLOCK_BUTTON in
	1) ~/.config/i3blocks/togle_bluetooth.sh
esac
exit





bluetoothctl show | grep "Powered: " | awk -v label=$LABEL -v connected=$connected '
{
	power=$2
	# full text
	print label" "
 	# short text
 	print label
}
END {
	if (power == "yes") {
 	   if (connected == "yes") {
			print "#66ffffff"
		}
	}
	else {
		print "#2193ff"
	}
}'
