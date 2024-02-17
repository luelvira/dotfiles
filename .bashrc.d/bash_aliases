# -*- mode: shell-script -*- 
# custom alias
alias xclip='xclip -sel clipboard'
alias su='su -'
alias icat="kitty +kitten icat"
if [ -f /usr/bin/batcat ]; then
	alias cat=batcat
fi

# alias for old command replacemnet 
#if [ -f /usr/bin/nvim ]; then
	#alias vi=nvim
#fi

if [ -f /usr/bin/lsd ]; then
	alias ls='lsd --group-dirs=first -F'
	alias ll='ls -al'
	alias la='ls -A'
	alias lha='ls -lhA'
elif [ -f /usr/bin/exa ]; then
	alias ls='exa --group-directories-first -F'
else
    alias ll='ls -alF'
    alias la='ls -A'
    alias l='ls -CF'
    alias lha='ls -lhA'
fi

if [ -f /usr/bin/batcat ]; then
    alias cat="batcat"
fi

# alias for git common commands
alias ggraph='git log --all --decorate --oneline --graph'
ginit() {
    mkdir "$1" && cd "$_" || exit
    echo "# $1" >> README.md
    git init
    git add README.md
    git commit -m "first commit"
    git branch -M main
    git remote add origin "git@github.com:luelvira/$1.git"
}
alias gsh='git push -u origin'
alias gl='git pull origin'
alias gss='git status'

# alias for custom functions
alias mkdir='mkdir -p'
tmptouch (){
	stamp=$(date +'%Y%m%d%H%M%s')
	touch "$1$stamp"
}
mkcd () {
	mkdir "$1" && cd "$_" || exit
}

tmpmkdir() {
	stamp=$(date +'%Y-%m-%d-%H.%M.%s')
	mkdir "$1$stamp"
}

gbackup() {
    stamp=$(date '+%Y-%m-%dT%H:%M')
    git commit -m $stamp
}
# make an alias to prevent accidental rm -rf
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'

# SHOPT (https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html)
shopt -s autocd
shopt -s dotglob


if [ -f "/usr/bin/nvim" ]; then
    alias vim="nvim"
fi

alias emt="/usr/bin/emacs -nw"
alias emc="emacsclient -c -a 'emacs'"
alias rem="killall emacs || echo 'Emacs server not running'; /usr/bin/emacs --daemon"
