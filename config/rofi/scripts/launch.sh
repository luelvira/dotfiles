#!/bin/bash

menu="$1"

if [ "$menu" = "filebrowser" ]; then
    rofi -modi "Filebrowser:~/.config/rofi/scripts/filebrowser.sh $2" -show Filebrowser 
elif [ "$menu" = "appmenu" ]; then
    rofi -show drun
elif [ "$menu" = "powermenu" ]; then
    rofi -modi 'Powermenu:~/.config/rofi/scripts/powermenu.sh' -show Powermenu -theme powermenu -location 3 -xoffset -28 -yoffset 50 -icon-theme "kora"
elif [ "$menu" = "tabmenu" ]; then
   rofi -show window
elif [ "$menu" = "run" ]; then
	rofi -show run 
elif [ "$menu" = "search" ]; then
    rofi -show 'search:~/.config/rofi/scripts/web-search.sh' -show search
fi
