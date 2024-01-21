#!/usr/bin/env sh
#/home/lucas/.local/bin/sync.sh
killall -9 emacs
/usr/bin/emacs --daemon && notify-send "Emacs ready"
