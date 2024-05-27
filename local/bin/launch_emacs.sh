#!/usr/bin/env sh
#/home/lucas/.local/bin/sync.sh
pkill emacs
while pkill -x emacs >/dev/null; do sleep 1; done
/usr/bin/emacs --daemon && notify-send "Emacs ready"
