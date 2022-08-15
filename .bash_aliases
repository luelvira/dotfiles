# custom alias
alias xclip='xclip -sel clipboard'
alias su='su -'
alias icat="kitty +kitten icat"
if [ -f /usr/bin/batcat ]; then
	alias cat=batcat
fi

alias ggraph='git log --all --decorate --oneline --graph'
