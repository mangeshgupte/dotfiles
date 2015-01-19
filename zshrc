#!/usr/bin/zsh

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mangesh"

HOSTNAME="`hostname`"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

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
plugins=(git pip history history-substring-search colorize hadoop jump)

# User configuration
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/mysql/bin"
export PYTHONPATH="${PYTHONPATH}:/houzz/c2/python:/houzz/c2/python/search_utils_server/houzz/services"

json_pretty_print() {
	python -m json.tool $1 | pygmentize -l json | less
}

# Use source highlighting with lesspipe.
LESSPIPE=`command -v src-hilite-lesspipe.sh`
if [[ ! -z "$LESSPIPE" ]]; then
	export LESSOPEN="| ${LESSPIPE} %s"
fi
# Case insensetive search for less.
export LESS='-Ri'

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
# 	echo 'remote hello'
# else
# 	echo 'local hello'
# fi

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# History
bindkey "^[^[[A" up-line-or-local-history    # [windows] + Cursor up
bindkey "^[^[[B" down-line-or-local-history  # [windows] + Cursor down

up-line-or-local-history() {
	zle set-local-history 1
	zle up-line-or-history
	zle set-local-history 0
}
zle -N up-line-or-local-history

down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history

# Set locale preferance.
# Impala Shell needs this otherwise it crashes on any unicode string.
if [[ ${(%):-%m} = stormsend ]]; then
	export LC_ALL=en_US.UTF-8
else
	export LC_ALL=en_US.UTF-8
fi

# aliases
[[ -f ~/.aliases.sh ]] && source ~/.aliases.sh

# Tab completion for marks
function _completemarks {
  reply=($(ls $MARKPATH))
}

compctl -K _completemarks jump
compctl -K _completemarks unmark

if [[ $HOSTNAME = stormsend.local ]]; then
    export PATH="/usr/local/bin:$PATH"
    export PATH="$PATH:$HOME/tools/arcanist/bin/"
    export WORKON_HOME=~/Envs
    source /usr/local/bin/virtualenvwrapper.sh
fi

source $ZSH/oh-my-zsh.sh

# -------------------------------------------------------------------
# display a neatly formatted path
# -------------------------------------------------------------------
path() {
  echo $PATH | tr ":" "\n" | \
    awk "{ sub(\"/usr\",   \"$fg_no_bold[green]/usr$reset_color\"); \
           sub(\"/bin\",   \"$fg_no_bold[cyan]/bin$reset_color\"); \
           sub(\"/opt\",   \"$fg_no_bold[blue]/opt$reset_color\"); \
           sub(\"/sbin\",  \"$fg_no_bold[magenta]/sbin$reset_color\"); \
           sub(\"/local\", \"$fg_no_bold[yellow]/local$reset_color\"); \
           print }"
}

