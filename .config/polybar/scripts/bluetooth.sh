#!/usr/bin/env bash

# if [ $(bluetoothctl info B0:18:6F:0B:1D:03 | grep Connected | awk '{print $2}') == "yes" ]; then
#    	echo "" 
# else
# 	echo ""
# fi

bluetooth_print() {
	if [ "$(systemctl is-active "bluetooth.service")" = "active" ] && bluetoothctl show | grep -e '^.Powered: yes' > /dev/null; then
		device_paired=$(bluetoothctl devices | cut -d ' ' -f 2)
		device_info=$(bluetoothctl info $device)
		if  echo "$device_info" | grep -e "^.Connected: yes" > /dev/null; then
			device_alias=$(echo "$device_info" | grep "Alias" | cut -d' ' -f 2-)
			output="%{F#81A1C1} %{F-}$device_alias"
		else
			output="%{F#81A1C1}"
		fi
	else
		output="%{F#808B96}"
	fi

	if ! [[ -n $last_output ]]; then
  	  echo $output
      last_output=$output
    elif [[ "$last_output" != "$output" ]]; then
      echo $output
      last_output=$output
    fi
}

bluetooth_toggle() {
	 devices_paired=$(bluetoothctl devices | cut -d ' ' -f 2)
	 if bluetoothctl info $devices_paired | grep -e "^.Connected: no" > /dev/null; then
	 	 bluetoothctl connect $devices_paired
	 else
	 	 bluetoothctl disconnect $devices_paired
	 fi
}

bluetooth_toggle_state() {
	if bluetoothctl show | grep -e '^.Powered: no' > /dev/null; then
		 bluetoothctl power on > /dev/null
	 else
	 	 bluetoothctl power off > /dev/null
	fi
}

case "$1" in
  --toggle)
    bluetooth_toggle
    ;;
	--state)
	bluetooth_toggle_state
	;;
  *)
    bluetooth_print
    ;;
esac

exit



powered=$(bluetoothctl show | grep "Powered: " | awk '{print $2}')
connected=$(bluetoothctl info B0:18:6F:0B:1D:03 | grep Connected | awk '{print $2}')
echo $LABEL
if [ $powered == "yes" ];then
	if [ $connected == "yes" ]; then
		conn=1
		echo \#2193ff
	else
		echo \#334d66
	fi
fi

case $BLOCK_BUTTON in
	1) ~/.config/i3blocks/togle_bluetooth.sh
esac
