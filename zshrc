# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mangesh"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias emacs=/Applications/Aquamacs.app/Contents/MacOS/Aquamacs
alias diffmerge=/Applications/DiffMerge.app/Contents/MacOS/DiffMerge
alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
alias grep="grep --color=always"
alias gitg="git grep --break --heading"

# Hadoop aliases
alias hfs="noglob hadoop fs"
alias hls="noglob hadoop fs -ls"
alias hrm="noglob hadoop fs -rm"
alias hcat="noglob hadoop fs -cat"
alias hmkdir="noglob hadoop fs -mkdir"

alias sshTunnel="ssh -L 8676:hdws01:3128 dw.houzz.net"

# Solr tunnels.
alias solrSpacesTunnel="ssh  -L 18994:stgdb01:8994  mangesh@stg.houzz.net -N"
alias solrKeywordTunnel="ssh  -L 18986:stgdb01:8986  mangesh@stg.houzz.net -N"
alias solrNERTunnel="ssh  -L 18987:stgdb01:8987  mangesh@stg.houzz.net -N"

# ssh aliases
alias hd="ssh hdwu01.hz"
alias stg="ssh stg.houzz.net"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git pip emacs history history-substring-search colorize hadoop)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/mysql/bin"

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
