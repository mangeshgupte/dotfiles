#!/usr/bin/zsh

# 'strict mode' so this fails fast.
# set -euo pipefail
# IFS=$'\n\t'

# Get a very simple prompt for dumb terminals. This helps with emacs tramp mode among other things.
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  if whence -w precmd >/dev/null; then
      unfunction precmd
  fi
  if whence -w preexec >/dev/null; then
      unfunction preexec
  fi
  PS1='$ '
fi

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

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export DOTFILES=$HOME/dotfiles
export ZSH_HOME=~/

if [[ $USER = hadoop ]]; then
    export ZSH=/home/mangesh/.oh-my-zsh
    export DOTFILES=/home/mangesh/dotfiles
    export ZSH_HOME=/home/mangesh
fi

export UPDATE_DOTFILES_DAYS=1
export DISABLE_UPDATE_PROMPT=1
# Check for dotfiles update on initial load...
env DOTFILES=$DOTFILES DISABLE_UPDATE_PROMPT=$DISABLE_UPDATE_PROMPT zsh -f $DOTFILES/utils/check_for_upgrade.sh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mangesh"

HOSTNAME="`hostname`"
SHORT_HOSTNAME=${(%):-%m}

# Example aliases

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

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
plugins=(git brew pip history history-substring-search autojump jump sudo dircycle zsh-syntax-highlighting)

# Use ipython where available
export IPYTHON=1

# Local aliases
if [[ $SHORT_HOSTNAME = 'PAC02LC18LFFT3' ]]; then
    # User configuration
    export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/bin:/Users/mangesh/Library/Python/2.7/bin:/Users/mangesh/Library/Python/3.7/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/mysql/bin:/Library/TeX/texbin:$PATH"
    export PYTHONIOENCODING='utf-8'
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
elif [[ $SHORT_HOSTNAME = 'data-util' ]]; then
    export ZSH='/home/mangesh/.oh-my-zsh'
    export SPARK_DIST_CLASSPATH=$(hadoop classpath)
    export ZSH_HOME=/home/mangesh

    unset IPYTHON
fi

# Add color to ls
export LS_OPTIONS="--color=auto"
if [[ "$(uname)" == "Darwin" ]]; then
    export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd
fi

# Add packages to the path if they exists.

# Anaconda
if [[ -d "$HOME/anaconda2" ]]; then
    export PATH="$HOME/anaconda2/bin:$PATH"
fi

# Arcanist
if [[ -d "$HOME/tools/arcanist/bin" ]]; then
    export PATH="$HOME/tools/arcanist/bin:$PATH"
fi

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

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=xterm-256color

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
[[ -f $ZSH_HOME/.aliases.sh ]] && source $ZSH_HOME/.aliases.sh

# Add a bookmarks functionality to zsh.
# Use: bm description. This will store
function bm() {
    let "index = $HISTCMD - 1"
    echo $history[$index] " # $@" >> ~/.bookmarks.zsh
}
