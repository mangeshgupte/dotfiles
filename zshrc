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
  return
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

export UPDATE_DOTFILES_DAYS=7
export DISABLE_UPDATE_PROMPT="true"
# Check for dotfiles update on initial load...
env DOTFILES=$DOTFILES DISABLE_UPDATE_PROMPT=$DISABLE_UPDATE_PROMPT zsh -f $DOTFILES/utils/check_for_upgrade.sh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mangesh"

local hostname=${(%):-%m}

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git
         history
         sudo
         dircycle
         zsh-syntax-highlighting
         zsh-history-substring-search
         history-search-multi-word
        )

# Use ipython where available
export IPYTHON=1

# Force path array to have unique values
typeset -U path PATH

# User configuration
# Local environment variables
local_machines=("starlight")

if [[ ${local_machines[(r)$hostname]} == $hostname ]]; then
    # $(brew --prefix coreutils) = /opt/homebrew/opt/coreutils. Replace if that changes.
    path=('/opt/homebrew/opt/coreutils/libexec/gnubin' $path)
    path+=('/usr/local/bin'
           '/usr/local/sbin'
           '/usr/bin'
           '/bin'
           '/usr/sbin'
           '/sbin'
           '/usr/local/mysql/bin'
           '/opt/homebrew/bin'
           '/Users/mangesh/Library/Python/3.9/bin'
           '/opt/homebrew/share/google-cloud-sdk/bin'
          )

    # Add packages to the path if they exists.
    local_directories=('.local' '.npm-global')
    for directory in $local_directories;
    do
        if [[ -d "$HOME/$directory" ]]; then
            path+=("$HOME/$directory/bin")
        fi
    done

    export PYTHONIOENCODING='utf-8'
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

if [[ "$(uname)" == "Darwin" ]]; then
    # export CLICOLOR=1
    # export LSCOLORS=GxFxCxDxBxegedabagaced
fi

setopt interactivecomments

json_pretty_print() {
	python -m json.tool $1 | pygmentize -l json | less
}

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

# Make less more readable
# Use source highlighting with source-highlight.
LESSPIPE=`command -v src-hilite-lesspipe.sh`
if [[ ! -z "$LESSPIPE" ]]; then
	export LESSOPEN="| ${LESSPIPE} %s"
fi

if type lesspipe.sh >/dev/null 2>&1; then
    export LESSOPEN='|lesspipe.sh %s'
fi

if type pygmentize >/dev/null 2>&1; then
  export LESSCOLORIZER='pygmentize'
fi


export LESS='--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD --tabs=4 --no-init --window=-4'

# aliases
[[ -f $ZSH_HOME/.aliases.sh ]] && source $ZSH_HOME/.aliases.sh

# Add a bookmarks functionality to zsh.
# Use: bm description. This will store
function bm() {
    let "index = $HISTCMD - 1"
    echo $history[$index] " # $@" >> ~/.bookmarks.zsh
}

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
    source /etc/profile.d/vte.sh
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/mangesh/miniforge3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/mangesh/miniforge3/etc/profile.d/conda.sh" ]; then
        . "/Users/mangesh/miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/mangesh/miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

. "$HOME/.local/bin/env"
