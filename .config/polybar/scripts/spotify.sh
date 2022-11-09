#!/usr/bin/bash


SPOTIFY_PID="$(pidof -s spotify)"
alias sp=/home/lucas/.local/bin/sp

case $1 in
	1)  
		if [[ -z "$SPOTIFY_PID" ]]; then
			flatpak run com.spotify.Client
		else
			/home/lucas/.local/bin/sp play
		fi
		;;

    3) home/lucas/.local/bin/sp next ;;
esac


# aquest script utiliza "sp"
# git clone https://gist.github.com/fa6258f3ff7b17747ee3.git

# ses dades són a partir de sa 2na columna
titol=$(/home/lucas/.local/bin/sp current | awk 'FNR==4 {first = $1; $1 = ""; print $0}' | sed 's/&/i/g')
artista=$(/home/lucas/.local/bin/sp current | awk 'FNR==3 {first = $1; $1 = ""; print $0}' | sed 's/&/i/g')

if [ "$titol" == "" ] && [ "$artist" == "" ]; then echo "" && exit;
else echo "  $titol -$artista"; fi;
