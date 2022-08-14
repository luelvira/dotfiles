function set_bash_prompt () {
	black="\[\033[0;30m\]"
	red="\[\033[0;31m\]"
	green="\[\033[0;32m\]"
	orange="\[\033[0;33m\]"
	blue="\[\033[0;34m\]"
	purple="\[\033[0;35m\]"
	cyan="\[\033[0;36m\]"
	gray="\[\033[0;37m\]"

	b_black="\[\033[1;30m\]"
	b_red="\[\033[1;31m\]"
	b_green="\[\033[1;32m\]"
	b_orange="\[\033[1;33m\]"
	b_blue="\[\033[1;34m\]"
	b_purple="\[\033[1;35m\]"
	b_cyan="\[\033[1;36m\]"
	b_gray="\[\033[1;37m\]"

	reset="\[\033[00m\]"



	#not working
	if test $? -eq 0 ; then
		path_color=${gray}
	else
		path_color=${red}
	fi

	if test $UID -eq 0 ; then
		split_color=${cyan}
		name_color=${red}
	else 
		split_color=${green}
		name_color=${cyan}
	fi
	git_color=${orange}

	if ! test -z "$VIRTUAL_ENV" ; then
		venv="${purple}(`basename \"$VIRTUAL_ENV\"`) "
	else
		venv=""
	fi

	# Window tittle
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]"
	# start PS1 prompt
	PS1="${PS1}${split_color}[${venv}${path_color}\w${split_color}]"

	if ! git_loc="$(type -p "$git_command_name")" || [ -z "$git_loc" ]; then
		# Git is installed
		if [ -d .git ] || git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
			# Inside of a git repository
			GIT_BRANCH=$(git symbolic-ref --short HEAD)
			PS1="${PS1} ${git_color}${GIT_BRANCH}"
		fi
	fi
	PS1="${PS1}\n${split_color}(${name_color}\u@\h${split_color})${name_color}"'\$ '

	PS1="${PS1}${reset}"
}

function tfg {
	if [ "$1" == "memoria" ]; then
		sudo systemctl start docker
		cd $clase/TFG/memoria/docker/
	elif [ "$1" == "code" ]; then
		sudo systemctl start mongod
		cd $clase/TFG/code/srcv1/
		activate
	else
		echo Missing argument
		cd $clase/TFG
	fi
}
# export PROMPT_COMMAND=set_bash_prompt

function activate {
	local venv=venv/bin/activate
	if  [ -f $venv ]; then
		source $venv
		
		if [ -f src/src-completion.bash ]; then
			source src/src-completion.bash
			echo "Load all configs"
		fi
	fi
}
