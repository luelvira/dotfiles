pkill polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar left -c ~/.config/polybar/config.ini &
polybar right -c ~/.config/polybar/config.ini &
