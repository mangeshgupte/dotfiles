# Local aliases
if [[ ${(%):-%m} = stormsend ]]; then
	alias emacs=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
	alias diffmerge=/Applications/DiffMerge.app/Contents/MacOS/DiffMerge
	alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
	alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
	alias c2="cd /houzz/c2"

	alias sshTunnel="ssh -L 8676:hdws01:3128 dw.houzz.net"

	# Solr tunnels.
	alias solrSpacesTunnel="ssh  -L 18994:stgdb01:8994  mangesh@stg.houzz.net -N"
	alias solrKeywordTunnel="ssh  -L 18986:stgdb01:8986  mangesh@stg.houzz.net -N"
	alias solrNERTunnel="ssh  -L 18987:stgdb01:8987  mangesh@stg.houzz.net -N"

	export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
	export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
fi

# Directory aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias c="clear"

LS_OPTIONS="--color=auto"
alias ls='ls $LS_OPTIONS'

alias k15="kill -15"

# git aliases
alias grep="grep --color=always"
alias gitg="git grep --break --heading --line-number"

# Hadoop aliases
alias hfs="noglob hadoop fs"
alias hls="noglob hadoop fs -ls"
alias hrm="noglob hadoop fs -rm"
alias hcat="noglob hadoop fs -cat"
alias hmkdir="noglob hadoop fs -mkdir"

# ssh aliases
alias dw="ssh hdwu01.hz"
alias stg="ssh stg.houzz.net"

