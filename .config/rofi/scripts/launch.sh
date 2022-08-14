#!/bin/bash

menu="$1"

if [ "$menu" = "filebrowser" ]; then
    rofi -modi "Filebrowser:~/.config/rofi/scripts/filebrowser.sh $2" -show Filebrowser -theme music-launcher
    
elif [ "$menu" = "appmenu" ]; then
    rofi -show drun -theme default
elif [ "$menu" = "powermenu" ]; then
#    rofi -modi 'Powermenu:~/scripts/rofi/powermenu.sh' -show Powermenu -theme powermenu
    #rofi -modi 'Powermenu:~/.config/rofi/scripts/powermenu.sh' -show Powermenu -theme powermenu -location 3 -xoffset -30 -yoffset 100 -icon-theme "kora"
    rofi -modi 'Powermenu:~/.config/rofi/scripts/powermenu.sh' -show Powermenu -theme powermenu -location 3 -xoffset -28 -yoffset 50 -icon-theme "kora"
elif [ "$menu" = "tabmenu" ]; then
   # ~/.config/rofi/scripts/tabmenu.sh
   rofi -show window -theme tabmenu
elif [ "$menu" = "run" ]; then
	rofi -show run 
fi
