#!/usr/bin/env bash

#
#echo ""
#if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]; then
#  echo \#66ffffff
#elif [ $(echo info | bluetoothctl | grep 'Device' | wc -c) -eq 0 ]; then
#  echo \#2193ff
#else
#	echo "#2193ff"
#fi
#
#
# /\/.*/ {
# 	gsub(/G$/,"", $3)
# 	# full text
# 	print label $3"/" $2
# 	# short text
# 	print label $4
# 	use=$5
# 	# no need to continue parsing
# 	exit 0
# }
# END {
# 	gsub(/%$/,"",use)
# 	if (100 - use < alert_low) {
# 		# color
# 		print "#FF0000"
# 	}
# }
# '

bluetoothctl show | grep "Powered: " | awk -v label=$LABEL '
{
	power=$2
	# full text
	print label" "
 	# short text
 	print label
}
END {
	if ($2 == "yes") {
		print "#66ffffff"
	}
	else {
		print "#2193ff"
	}
}'
