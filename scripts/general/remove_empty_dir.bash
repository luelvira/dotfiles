#!/usr/bin/env bash

remove_dir() {
	# Scape white space
	local cdir=$(echo ${1/ \\ })
	if [ -d "$cdir" ]; then
		ls -A1 "$cdir" | while read f
		do
			echo "call remove_dir with $cdir/$f"
			remove_dir "$cdir/$f"
		done
		echo "remove $cdir"
		rmdir "$cdir"
	fi
}

remove_dir "./$1"
