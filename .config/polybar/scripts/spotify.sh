#!/usr/bin/bash


SPOTIFY_PID="$(pidof -s spotify)"

case $1 in
	1)  
		if [[ -z "$SPOTIFY_PID" ]]; then
			flatpak run com.spotify.Client
		else
			sp play
		fi
		;;

    3) sp next ;;
esac


# aquest script utiliza "sp"
# git clone https://gist.github.com/fa6258f3ff7b17747ee3.git

# ses dades són a partir de sa 2na columna
titol=$(sp current | awk 'FNR==4 {first = $1; $1 = ""; print $0}' | sed 's/&/i/g')
artista=$(sp current | awk 'FNR==3 {first = $1; $1 = ""; print $0}' | sed 's/&/i/g')

if [ "$titol" == "" ] && [ "$artist" == "" ]; then echo "" && exit;
else echo "  $titol -$artista"; fi;
