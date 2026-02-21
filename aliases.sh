# Local aliases
short_hostname=${(%):-%m}

alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"

# Common programs
alias m="mysql"

# Directory aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias c="clear"

alias ls='ls -p --color=auto'
alias l='ls -lhS $LS_OPTIONS'

alias k15="kill -15"

# grep aliases
unset GREP_OPTIONS;
alias grep="grep --color=always"
alias grepp="grep"
alias gitg="git grep --break --heading --line-number"


# Aliases for most used git commands. Used this to get this list :
# cat ~/.zsh_history|cut -d ';' -f 2- 2>/dev/null|grep "^git"|sort|uniq -c|sort -nr | head -20
alias ga="git add"
compdef _git ga=git-add

alias gs="git status"
compdef _git gs=git-status

alias gl="git log"
compdef _git gl=git-log

alias gd="git diff"
compdef _git gd=git-diff

alias gp="git pull origin master"
compdef _git gd=git-pull

alias gco="git checkout"
compdef _git gco=git-checkout

# Enable color highlighting in tmux
alias tmux="tmux -2"
alias ta="tmux -2 attach"

# Lookup the most frequenty used commands.
# alias freq=cat ~/.zsh_history|cut -d ';' -f 2- 2>/dev/null|awk '{a[$1]++ } END{for(i in a){print a[i] " " i}}'|sort -rn|head -25
# Neat commands

function histogram() {
    awk '{print $1}' | sort | uniq -c | sort -rn | head -50 | awk '!max{max=$1;}{r="";i=s=60*$1/max;while(i-->0)r=r"#";printf "%15s %5d %s %s",$2,$1,r,"\n";}'
}
