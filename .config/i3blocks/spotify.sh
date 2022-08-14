#!/usr/bin/bash


SPOTIFY_PID="$(pidof -s spotify)"
if [[ -z "$SPOTIFY_PID" ]]; then
	echo ""
	case $BLOCK_BUTTON in
		1) i3-msg -q -- exec flatpak run com.spotify.Client
	esac
fi

case $BLOCK_BUTTON in
	1) sp play ;;

    3) sp next ;;
esac


# aquest script utiliza "sp"
# git clone https://gist.github.com/fa6258f3ff7b17747ee3.git

# ses dades són a partir de sa 2na columna
titol=$(sp current | awk 'FNR==4 {first = $1; $1 = ""; print $0}' | sed 's/&/i/g')
artista=$(sp current | awk 'FNR==3 {first = $1; $1 = ""; print $0}' | sed 's/&/i/g')

if [ "$titol" == "" ] && [ "$artist" == "" ]; then echo "" && exit;
else echo "$titol -$artista"; fi;
