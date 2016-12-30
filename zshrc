#!/usr/bin/zsh

# 'strict mode' so this fails fast.
# set -euo pipefail
# IFS=$'\n\t'

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mangesh"

HOSTNAME="`hostname`"
SHORT_HOSTNAME=${(%):-%m}

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
plugins=(git brew pip history history-substring-search autojump jump zsh-syntax-highlighting z sudo dircycle)

# Local aliases
if [[ $SHORT_HOSTNAME = 'PA-MBP-C02LC18LFFT3' ]]; then
    # User configuration
    export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/mysql/bin:$PATH"
    export PYTHONPATH="/houzz/c2/python_home:/houzz/c2/python_home/houzz/search_utils_server/services"
    export PYTHONIOENCODING='utf-8'

    export GOPATH="$HOME/Go"
    export GOROOT="/usr/local/opt/go/libexec"
    export PATH="$PATH:$GOPATH/bin:$GOROOT/bin:$HOME/bin"

    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
elif [[ $SHORT_HOSTNAME = 'hdwu01' ]]; then
    export WORKON_HOME=$HOME/.virtualenvs
    export PROJECT_HOME=$HOME/Devel
    # export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
    # source /usr/local/bin/virtualenvwrapper_lazy.sh
fi

# Use ipython where available
export IPYTHON=1

setopt INTERACTIVE_COMMENTS

json_pretty_print() {
	python -m json.tool $1 | pygmentize -l json | less
}


# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
# 	echo 'remote hello'
# else
# 	echo 'local hello'
# fi

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"


# Set locale preferance.
if [[ ${(%):-%m} = stormsend ]]; then
    export PATH="$PATH:$HOME/tools/arcanist/bin"
    export WORKON_HOME=~/Envs
    source /usr/local/bin/virtualenvwrapper.sh
    # Impala Shell needs this otherwise it crashes on any unicode string.
    export LC_ALL=en_US.UTF-8
else
	export LC_ALL=en_US.UTF-8
fi

# Tab completion for marks
function _completemarks {
  reply=($(ls $MARKPATH))
}

compctl -K _completemarks jump
compctl -K _completemarks unmark

source $ZSH/oh-my-zsh.sh

# Use source highlighting with source-highlight.
LESSPIPE=`command -v src-hilite-lesspipe.sh`
if [[ ! -z "$LESSPIPE" ]]; then
	export LESSOPEN="| ${LESSPIPE} %s"
fi

# i: Case insensetive search for less.
# R:
# F: Quit if
# X: Keep screen.
export LESS=' -RXFi '

# aliases
[[ -f ~/.aliases.sh ]] && source ~/.aliases.sh


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


# Install powerline
# function powerline_precmd() {
#     export PS1="$(/Users/mangesh/utils/powerline-shell/powerline-shell.py $? --shell zsh 2> /dev/null)"
# }

# function install_powerline_precmd() {
#     for s in "${precmd_functions[@]}"; do
#         if [ "$s" = "powerline_precmd" ]; then
#             return
#         fi
#     done
#     precmd_functions+=(powerline_precmd)
# }

# install_powerline_precmd

# Add a bookmarks functionality to zsh.
# Use: bm description. This will store
function bm() {
    let "index = $HISTCMD - 1"
    echo $history[$index] " # $@" >> ~/.bookmarks.zsh
}

