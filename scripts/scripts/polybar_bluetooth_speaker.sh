#!/bin/bash

source colors.sh

bluetooth_speaker() {
	SPEAKER_CONNECTED=$(bt-device -i 78:44:05:F6:E9:15 | grep Connected | xargs | cut -d ' ' -f 2)
	if [ "$SPEAKER_CONNECTED" -eq 1 ]; then
		speaker_indicator="${faded_green}OK${RESET}"
	else
		speaker_indicator="${dark0_soft}FAIl${RESET}"
	fi
	echo $speaker_indicator
}
bluetooth_speaker

