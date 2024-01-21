#!/usr/bin/env bash

bluetooth_print() {
    if [ "$(systemctl is-active "bluetooth.service")" = "active" ] && \
           bluetoothctl show | grep -e '^.Powered: yes' > /dev/null; then
        device_info=$(bluetoothctl info)
        if  echo "$device_info" | grep -e "^.Connected: yes" > /dev/null; then
            device_alias=$(echo "$device_info" | grep "Alias" | cut -d' ' -f 2-)
            output="%{F#81A1C1} %{F-}$device_alias"
        else
            output="%{F#81A1C1}󰂯"
        fi
    else
        output="%{F#808B96}󰂯"
    fi

    if [[ -z "$last_output" ]]; then
        echo "$output"
        last_output=$output
    elif [[ "$last_output" != "$output" ]]; then
        echo "$output"
        last_output=$output
    fi
}

bluetooth_toggle() {
    devices_paired=$(bluetoothctl devices | tail -n 1 | cut -d ' ' -f 2)
    if bluetoothctl info "$devices_paired" | grep -e "^.Connected: no"; then
        bluetoothctl connect "$devices_paired"
    else
        bluetoothctl disconnect "$devices_paired"
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
