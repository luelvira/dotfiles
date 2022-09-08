# custom alias
alias xclip='xclip -sel clipboard'
alias su='su -'
alias icat="kitty +kitten icat"
if [ -f /usr/bin/batcat ]; then
	alias cat=batcat
fi

if [ -f /usr/bin/nvim ]; then
	alias vi=nvim
fi

if [ -f /usr/bin/lsd ]; then
	alias ls='lsd --group-dirs=first -F'
	alias ll='ls -al'
	alias la='ls -A'
	alias lha='ls -lhA'
fi

alias ggraph='git log --all --decorate --oneline --graph'
alias mkdir='mkdir -p'
tmptouch (){
	stamp=$(date +'%Y%m%d%H%M%s')
	touch "$1$stamp"
}
mkcd () {
	mkdir $1 && cd $_
}
ginit() {
	mkdir $1 && cd $_
	git init
}

