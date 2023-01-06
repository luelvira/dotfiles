#!/usr/bin/env bash

query=$( (echo ) | rofi -theme whale -dmenu -i -lines 0 -matching fuzzy -location 0 -p " " )

if [[ -n "$query" ]]; then
    # https://scholar.google.com/scholar?hl=en&q=
  url=https://www.duckduckgo.com/?q=$query
  xdg-open "$url"
else
  exit
fi

exit 0
