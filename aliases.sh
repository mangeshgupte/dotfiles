# Local aliases
short_hostname=${(%):-%m}
if [[ $short_hostname = 'PAC02LC18LFFT3' ]]; then
	alias emacs=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
	alias diffmerge=/Applications/DiffMerge.app/Contents/MacOS/DiffMerge
	alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
	alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
	alias c2="cd /houzz/c2"

    alias tunnelDw="ssh -L 8676:hdws01.hz:3128 -L 21050:hdwu01.hz:21050 hdwu01.hz"

    # Staging tunnels.
    alias tunnelStg='ssh -L 18994:solr.hza:8994 `#SpacesNew` -L 18980:solr02:8980 `#Photos` -L 18981:solr.hza:8981 `#Products` -L 18986:solr02.hza:8986 `#Keywords` -L 18987:solr02.hza:8987 `#NER`  -L 18993:solr02.hza:8993 `#Answers` -L 18995:solr02.hza:8995 `#UsersNew` -L 18988:solr02.hza:8988 `#Ads` -L 13306:mysql-master:3306 `#mysql-master` -L 23306:mysql-slave:3306 `#mysql-slave` -L 16379:redis:6379 `#redis_main_and_feed` -L 18379:redis:18379 `#redis_cached_houses` stghouzz.hza'

    alias tunnelStgBig="ssh -L 18994:solr:8994 `#SpacesNew` -L 18980:solr02:8980 `#Photos` -L 18981:solr:8981 `#Products` -L 18986:solr02:8986 `#Keywords` -L 18987:solr02:8987 `#NER`  -L 18993:solr02:8993 `#Answers` -L 18995:solr02:8995 `#UsersNew` -L 18988:solr02:8988 `#Ads` -L 13306:mysql-master:3306 `#mysql-master` -L 23306:mysql-slave:3306 `#mysql-slave` -L 16379:redis:6379 `#redis_main_and_feed` -L 18379:redis:18379 `#redis_cached_houses` bastion.staging.houzz.net"

    export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"
    export PATH="/houzz/c2ubuntu/tools/cluster_access:$PATH"
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
fi

alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"

# Common programs
alias m="mysql"

# Read logs
alias logc2error='ls -t1 /houzz/c2/log/error_* | head -n1 | xargs tail -n200 -f'
alias logc2='ls -t1 /houzz/c2/log/log_* | head -n1 | xargs tail -n200 -f'
alias logc2common='ls -t1 /houzz/c2/log/common_log_* | head -n1 | xargs tail -n200 -f'

# Directory aliases
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias c="clear"

alias ls='ls -hF $LS_OPTIONS'
alias l='ls -lhS $LS_OPTIONS'

alias k15="kill -15"

# grep aliases
unset GREP_OPTIONS;
alias grep="grep --color=always"
alias grepp="grep"
alias gitg="git grep --break --heading --line-number"

HADOOP_SERVERS=(hdwu01 hdws01 data-util)
if [[ ${HADOOP_SERVERS[*]} =~ $short_hostname ]]; then
    # Hadoop aliases
    alias hfs="noglob hadoop fs"
    alias hls="noglob hadoop fs -ls"
    alias hcp="noglob hadoop fs -cp"
    alias hrm="noglob hadoop fs -rm"
    alias hget="noglob hadoop fs -get"
    alias hput="noglob hadoop fs -put"
    alias hcat="noglob hadoop fs -cat"
    alias hmkdir="noglob hadoop fs -mkdir"
    alias hrmdir="noglob hadoop fs -rmdir"
    # alias h="noglob hadoop"

    # Snakebite aliases
    alias fls="noglob snakebite ls"
    alias frm="noglob snakebite rm"
    alias fcp="noglob snakebite cp"
    alias fcat="noglob snakebite cat"
    alias fmkdir="noglob snakebite mkdir"
    alias ftail="noglob snakebite tail"
    alias fchmod="noglob snakebite chmod"
    alias fget="noglob snakebite get"
    alias fput="noglob snakebite put"

    alias ss="/home/hadoop/spark-2.1.1/spark-2.1.1-bin-hadoop2.6/bin/spark-submit"
fi

# ssh aliases
alias dw="ssh data-util.hzd"
alias dws="ssh hdwu01.hz"
alias stg="ssh stghouzz.hza"

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

# This needs icdiff to be installed.
alias gdi="git difftool --no-prompt --extcmd=icdiff"
compdef _git gdi=git-diff

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
