#!/usr/bin/env bash
DIR="${DIR:-$BLOCK_INSTANCE}"
DIR="${DIR:-$HOME}"
ALERT_LOW="${ALERT_LOW:-$1}"
ALERT_LOW="${ALERT_LOW:-10}" # color will turn red under this value (default: 10%)

LOCAL_FLAG="-l"
if [ "$1" = "-n" ] || [ "$2" = "-n" ]; then
    LOCAL_FLAG=""
fi

df -h -P $LOCAL_FLAG "$DIR" | awk -v label="$LABEL" -v alert_low=$ALERT_LOW '
/\/.*/ {
	gsub(/G$/,"", $3)
	# full text
	print label $3"/" $2
	# short text
	print label $4
	use=$5
	# no need to continue parsing
	exit 0
}
END {
	gsub(/%$/,"",use)
	if (100 - use < alert_low) {
		# color
		print "#FF0000"
	}
}
'

