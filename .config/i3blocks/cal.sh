#!/usr/bin/bash

file=/tmp/datei3blocks
if [ -f $file ]; then
	date=$(date +"%b %d, %Y")
	time=$(date +%H:%M:%S)
else
	date=""
	time=$(date +%H:%M)
fi
echo "$date   $time"

case $BLOCK_BUTTON in 
	1) [ -f $file ] && rm $file || touch $file
esac


