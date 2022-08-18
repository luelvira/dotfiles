# custom alias
alias xclip='xclip -sel clipboard'
alias su='su -'
alias icat="kitty +kitten icat"
if [ -f /usr/bin/batcat ]; then
	alias cat=batcat
fi

if [ -f /usr/bin/lsd ]; then
	alias ls='lsd --group-dirs=first -F'
	alias ll='ls -al'
	alias la='ls -A'
	alias lha='ls -lhA'
fi

alias ggraph='git log --all --decorate --oneline --graph'
alias vi=nvim
alias mkdir='mkdir -p'
alias mkcd='mkdir && cd $_'
