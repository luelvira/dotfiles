#!/bin/bash
#
# Custom clock/calendar for polybar.
#
# Author: machaerus
# https://gitlab.com/machaerus


dark0_hard=%{F#121519}
dark0_soft=%{F#29313c}
light0_hard=%{F#eaeadd}
light0_soft=%{F#d3ccc4}

faded_red=%{F#FD3044}
faded_green=%{F#946D5A}
faded_yellow=%{F#C49965}
faded_blue=%{F#EFB590}
faded_purple=%{F#D23243}
faded_aqua=%{F#A2817D}

bright_red=%{F#ff2a48}
bright_green=%{F#cf8865}
bright_yellow=%{F#ffc86f}
bright_blue=%{F#ffe5a2}
bright_purple=%{F#ff3049}
bright_aqua=%{F#e5998f}

RESET=%{F-}

custom_time() {
	WEEKDAY=$(date +%A)
	TIME=$(date +%H:%M:%S)
	DATE=$(date +"%e %b %Y")

	echo "$light0_soft $WEEKDAY $faded_yellow $TIME $light0_soft $DATE $RESET"
}

custom_time
