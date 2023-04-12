#!/usr/bin/env bash

# This script is used to make easy to switch between projects. A project is defined
# by a directory that contains a .git directory. 
# The script info is stored in the home directory in a folder called .projects.d/
# The file is a simple csv file with one line per project. The first column is the
# name of the project and the second column is the path to the project directory.
# The third column is the last accessed date and time in the format YYYY-MM-DD-HH:MM:SS
# The fourth column is the number of times the project has been accessed.
# To change the sort order or the directory used to check the projects, change the
# variables below.
# Keep in mind that, if you change the method used to check the project, the script
# will use a recursive search to find your file, but with git, it will only check
# if the directory is in a git repository.

# The directory to check for projects
PROJECT_DIR=~/.projects.d
PROJECT_METHOD=git
PROJECT_SORT=time

# The file to store the project info
PROJECT_FILE="$PROJECT_DIR/projects.csv"

function searchProject() {

	if [ ! -d ~/.projects.d ]; then
		mkdir ~/.projects.d
	fi

	if [ ! -f $PROJECT_FILE ]; then
		touch $PROJECT_FILE
	fi

	# Check if the project is already in the list
	# If it is, update the last accessed date and time and increment the access count
	# If it is not, add it to the list

	if [ "$PROJECT_METHOD" == "git" ]; then
		PROJECT_PATH=$(git rev-parse --show-toplevel 2>/dev/null)
	else 
		cpath=$(pwd)
		while [ ! -f $PROJECT_METHOD || ! -d $PROJECT_METHOD || ]; do
			if [ $(pwd) == "/" ]; then
				exit 0
			fi
			cd ..
		done
		PROJECT_PATH=$(pwd)
		cd $cpath
	fi

	if [ -z $PROJECT_PATH ]; then
		exit 0
	fi

	# check if the user is navigating to a subdirectory of the project
	# if so, then change the project path to the parent directory
	# this is done to avoid having multiple entries for the same project

	if [ -f /tmp/last_project ]; then
		local last_project=$(cat /tmp/last_project)
		if [ ! -z $last_project ]; then
			if [[ "$PROJECT_PATH" == "$last_project" ]]; then
				exit 0
			fi
		fi
	fi
	echo $PROJECT_PATH > /tmp/last_project
	

	if [ -z $(grep $PROJECT_PATH $PROJECT_FILE) ]; then
		PROJECT_NAME=$(basename $PROJECT_PATH)
		echo "$PROJECT_NAME,$PROJECT_PATH,$(date +%Y-%m-%d-%H:%M:%S),1" >> $PROJECT_FILE
	else
		PROJECT_NAME=$(grep $PROJECT_PATH $PROJECT_FILE | cut -d, -f1)
		PROJECT_PATH=$(grep $PROJECT_PATH $PROJECT_FILE | cut -d, -f2)
		PROJECT_COUNT=$(grep $PROJECT_PATH $PROJECT_FILE | cut -d, -f4)
		PROJECT_COUNT=$((PROJECT_COUNT+1))
		PROJECT_TIME=$(date +%Y-%m-%d-%H:%M:%S)
		NEW_LINE="$PROJECT_NAME,$PROJECT_PATH,$PROJECT_TIME,$PROJECT_COUNT"
		sed -i "s#^$PROJECT_NAME,$PROJECT_PATH.*#$NEW_LINE#" $PROJECT_FILE
	fi
}

function listProjects() {
	# This is the main function. First clean the screen and get the terminal size
	# Then, prepare the menu and display it

	get_term_size
	setup_terminal


	get_menu
	# Get the number of lines in the menu
	# If the number of lines is greater than the max number of items that can be displayed
	# then set the number of lines to the max number of items
	MAX_LINES=$(echo "$menu" | wc -l)
	if [ $MAX_LINES -gt 10 ]; then
		MAX_LINES=10
	fi

	selected=0
	screen=0
	while true; do
		local idx=0
printf "________             ________           __________              \n"
printf "___  __ \__________________(_)____________  /___(_)____________ \n"
printf "__  /_/ /_  ___/  __ \____  /_  _ \  ___/  __/_  /_  __ \_  __ \\n"
printf "_  ____/_  /   / /_/ /___  / /  __/ /__ / /_ _  / / /_/ /  / / /\n"
printf "/_/     /_/    \____/___  /  \___/\___/ \__/ /_/  \____//_/ /_/ \n"
printf "                     /___/                                      \n"
		printf "Select a project:\n" 

		for line in $menu; do
			local name=$(echo $line | cut -d, -f1)
			local path=$(echo $line | cut -d, -f2 | sed "s#$HOME#~#")
			local time=$(echo $line | cut -d, -f3)
			local count=$(echo $line | cut -d, -f4)
			local length=$(echo $path | wc -c)
			if [ $length -gt 40 ]; then
				IFS='/' read -ra ADDR <<< "$path"
				path="~/${ADDR[1]}/.../${ADDR[-2]}/${ADDR[-1]}"
			fi
			if [ $idx -eq $selected ]; then
				printf '[%d] \033[1;32m%-20s\033[0m %-40s %s %s' $idx "$name" "$path" "$time" "$count"
			else
				printf '[%d] %-20s %-40s %s %s' $idx "$name" "$path" "$time" "$count"
			fi
			printf '\n'
			idx=$((idx+1))
			if [ $idx -eq $MAX_LINES ]; then
				break
			fi
		done
		printf '\n\n'
		for ((i=0; i<120; i++)); do
			printf '-'
		done
		printf '\n'
		printf 'Press [up] and [down] to change project\n'
		printf 'Press [left] and [right] to change screen\n'
		#printf 'Press [enter] to select project\n'
		if [ $screen -eq 0 ]; then
			printf 'Press [t] to sort by time\n'
			printf 'Press [f] to sort by frequency\n\n'
		elif [ $screen -eq 1 ]; then
			printf 'Press [f] to forget project\n'
			printf 'Press [d] to delete project\n\n'
		fi
			
		printf 'Press [q] to quit\n'
		# Action on key press



		esc=$(printf "\033")
		read -rsn1 read_reply # read one key
		if [[ $read_reply == $esc ]]; then # if the key is esc then read the next two chars
			read -rsn2 read_reply
		fi
		# check if the key is a number
		if [[ $read_reply =~ ^[0-9]+$ ]]; then
			selected=$read_reply
			break
		fi
		case $read_reply in
			'[A') # up
				selected=$((selected-1))
				if [ $selected -lt 0 ]; then
					selected=$((MAX_LINES-1))
				fi
				;;
			'[B') # down
				selected=$((selected+1))
				if [ $selected -eq $MAX_LINES ]; then
					selected=0
				fi
				;;
			'[C') # right
				screen=$((screen+1))
				if [ $screen -eq 2 ]; then
					screen=1
				fi
				;;
			'[D') # left
				screen=$((screen-1))
				if [ $screen -eq -1 ]; then
					screen=0
				fi
				;;
			$'\x0a') # enter
				break
				;;
			"") break;;
			't')
				PROJECT_SORT=time
				get_menu
				;;
			'f')
				if [ $screen -eq 0 ]; then
					PROJECT_SORT=count
					get_menu
				elif [ $screen -eq 1 ]; then
					forgetProject
				fi
				;;
			'q')  
				selected=-1
				break;;
			'Q')  
				selected=-1
				break;;
		esac
		clear_screen
	done

	reset_terminal
	if [ ! $selected -eq -1 ]; then
		idx=0
		for line in $menu; do
			if [ $idx -eq $selected ]; then
				local name=$(echo $line | cut -d, -f1)
				local path=$(echo $line | cut -d, -f2)
				cd $path
			fi
			idx=$((idx+1))
		done
	fi
}

function forgetProject() {
	idx=0
	for line in $menu; do
		if [ $idx -eq $selected ]; then
			local name=$(echo $line | cut -d, -f1)
			local path=$(echo $line | cut -d, -f2)
			sed -i "/$name/d" $PROJECT_FILE
		fi
		idx=$((idx+1))
	done
	get_menu
	
}
function setup_terminal() {
	# Setup the terminal for the tui
    # '\e[?1049h': Use alternative screen buffer.
    # '\e[?7l':    Disable line wrapping.
    # '\e[?25l':   Hide the cursor.
    # '\e[2J':     Clear the screen.
    # '\e[1;Nr':   Limit scrolling to scrolling area.
    #              Also sets cursor to (0,0).
    printf '\e[?1049h\e[?7l\e[?25l\e[2J\e[1;%sr' "$max_items"

    # Hide echoing of user input
    stty -echo
}

function reset_terminal() {
    # Reset the terminal to a useable state (undo all changes).
    # '\e[?7h':   Re-enable line wrapping.
    # '\e[?25h':  Unhide the cursor.
    # '\e[2J':    Clear the terminal.
    # '\e[;r':    Set the scroll region to its default value.
    #             Also sets cursor to (0,0).
    # '\e[?1049l: Restore main screen buffer.
    printf '\e[?7h\e[?25h\e[2J\e[;r\e[?1049l'

    # Show user input.
    stty echo
}


function clear_screen() {
    # Only clear the scrolling window (dir item list).
    # '\e[%sH':    Move cursor to bottom of scroll area.
    # '\e[9999C':  Move cursor to right edge of the terminal.
    # '\e[1J':     Clear screen to top left corner (from cursor up).
    # '\e[2J':     Clear screen fully (if using tmux) (fixes clear issues).
    # '\e[1;%sr':  Clearing the screen resets the scroll region(?). Re-set it.
    #              Also sets cursor to (0,0).
    printf '\e[%sH\e[9999C\e[1J%b\e[1;%sr' \
           "$((LINES-2))" "${TMUX:+\e[2J}" "$max_items"
}

function get_term_size() {
    # Get terminal size ('stty' is POSIX and always available).
    # This can't be done reliably across all bash versions in pure bash.
    read -r LINES COLUMNS < <(stty size)

    # Max list items that fit in the scroll area.
    ((max_items=LINES-3))
}

function get_menu() {
	if [ "$PROJECT_SORT" == "time" ]; then
		menu=$(sort -t, -k3 -r $PROJECT_FILE)
	elif [ "$PROJECT_SORT" == "count" ]; then
		menu=$(sort -t, -k4 -r -n $PROJECT_FILE)
	else
		menu=$(cat $PROJECT_FILE)
	fi
}



function main() {
	if [ -z $1 ]; then
		listProjects
	else
		case $1 in
			-s)
				searchProject
				;;
			*)
				echo "Invalid option"
				;;
		esac
	fi
}

main $1


