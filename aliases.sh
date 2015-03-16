# Local aliases
if [[ ${(%):-%m} = stormsend ]]; then
	alias emacs=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
	alias diffmerge=/Applications/DiffMerge.app/Contents/MacOS/DiffMerge
	alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
	alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
	alias c2="cd /houzz/c2"

    alias tunnelDW="ssh -L 8676:hdws01:3128 dw.houzz.net"

	# Solr tunnels.
    alias tunnelSolrSpaces="ssh -L 18994:stgdb01:8994  mangesh@stg.houzz.net -N"
    alias tunnelSolrKeywords="ssh -L 18986:stgdb01:8986  mangesh@stg.houzz.net -N"
    alias tunnelSolrNER="ssh -L 18987:stgdb01:8987  mangesh@stg.houzz.net -N"
    alias tunnelStgDB="ssh -L 13306:stgdb01:3306  mangesh@stg.houzz.net -N"
    alias tunnelStgCat="ssh -L 55001:stgdb02:55000  mangesh@stg.houzz.net -N"

    export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
    export PATH="/houzz/c2ubuntu/tools/cluster_access:$PATH"
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
fi

alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"

# Directory aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias c="clear"

LS_OPTIONS="--color=auto"
alias ls='ls $LS_OPTIONS'
alias l='ls -lh $LS_OPTIONS'

alias k15="kill -15"

# grep aliases
alias grep="grep --color=always"
alias grepp="grep"
alias gitg="git grep --break --heading --line-number"

# Hadoop aliases
alias hfs="noglob hadoop fs"
alias hls="noglob hadoop fs -ls"
alias hrm="noglob hadoop fs -rm"
alias hcat="noglob hadoop fs -cat"
alias hmkdir="noglob hadoop fs -mkdir"

# ssh aliases
alias dw="ssh hdwu01.hz"
alias stg="ssh stgweb02.hzs"

# Aliases for most used git commands. Used this to get this list :
# cat ~/.zsh_history|cut -d ';' -f 2- 2>/dev/null|grep "^git"|sort|uniq -c|sort -nr | head -20
alias ga="git add"
compdef _git gs=git-add

alias gs="git status"
compdef _git gs=git-status

alias gl="git log"
compdef _git gl=git-log

alias gd="git diff"
compdef _git gd=git-diff

# This needs icdiff to be installed.
alias gdi="git difftool --no-prompt --extcmd=icdiff"
compdef _git gdi=git-diff

alias gp="git pull origin master"
compdef _git gd=git-pull

alias gco="git checkout"
compdef _git gco=git-checkout


# Lookup the most frequenty used commands.
# alias freq=cat ~/.zsh_history|cut -d ';' -f 2- 2>/dev/null|awk '{a[$1]++ } END{for(i in a){print a[i] " " i}}'|sort -rn|head -25
