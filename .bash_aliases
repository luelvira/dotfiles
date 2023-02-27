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
fi

if [ -f /usr/bin/exa ]; then
	alias ls='exa --group-directories-first -F'
	alias ll='ls -al'
	alias la='ls -A'
	alias lha='ls -lhA'
fi


# alias for git common commands

alias ggraph='git log --all --decorate --oneline --graph'
ginit() {
	mkdir $1 && cd $_
	echo "# $1" >> README.md
	git init
	git add README.md
	git commit -m "first commit"
	git branch -M main
	git remote add origin "git@github.com:luelvira/$1.git"

}

# alias for custom functions
alias mkdir='mkdir -p'
tmptouch (){
	stamp=$(date +'%Y%m%d%H%M%s')
	touch "$1$stamp"
}
mkcd () {
	mkdir $1 && cd $_
}

tmpmkdir() {
	stamp=$(date +'%Y-%m-%d-%H.%M.%s')
	mkdir "$1$stamp"
}

gbackup() {
    stamp=$(date '+%Y-%m-%dT%H:%m')
    git commit -m $stamp
}

# make an alias to prevent accidental rm -rf
alias rm='rm -I'
alias emacs='emacsclient --create-frame --alternate-editor=""'
