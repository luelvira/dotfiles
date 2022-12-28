pkill polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# check if the first argument is bspwd. Else load i3 config

if [ "$1" = "bspwm" ]; then
#	bars="keyboard workspace date audio workspace2 uptime ssh ip ram"
#	for bar in $bars; do
#		polybar $bar --config=~/.config/polybar/config-bspwm.ini &
#	done
	polybar left --config=~/.config/polybar/config-bspwm.ini &
	polybar right --config=~/.config/polybar/config-bspwm.ini &
else
	polybar left --config=~/.config/polybar/config.ini &
	polybar right --config=~/.config/polybar/config.ini &
fi

#polybar second --config=~/.config/polybar/config.ini &
