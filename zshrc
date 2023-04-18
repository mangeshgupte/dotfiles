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

if [[ $USER = hadoop ]]; then
    export ZSH=/home/mangesh/.oh-my-zsh
    export DOTFILES=/home/mangesh/dotfiles
    export ZSH_HOME=/home/mangesh
fi

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
plugins=(git history sudo dircycle zsh-syntax-highlighting zsh-history-substring-search history-search-multi-word calc)

# Use ipython where available
export IPYTHON=1

# User configuration
# Local environment variables
local_machines=("PAC02LC18LFFT3" "PA-MBP-C02LC18LFFT3" "garfield" "PAC02C10WXMD6P")

if [[ ${local_machines[(r)$hostname]} == $hostname ]]; then
    # $(brew --prefix coreutils) = /usr/local/opt/coreutils. Replace if that changes.
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/bin:/Users/mangesh/Library/Python/2.7/bin:/Users/mangesh/Library/Python/3.7/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/mysql/bin:/Library/TeX/texbin:/houzz/c2ubuntu/tools/cluster_access:/home/mangesh/.npm-global/bin:$PATH"
    export PYTHONIOENCODING='utf-8'
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
elif [[ $hostname = 'data-util' ]]; then
    export ZSH='/home/mangesh/.oh-my-zsh'
    export SPARK_DIST_CLASSPATH=$(hadoop classpath)
    export ZSH_HOME=/home/mangesh
    export ZSH_DISABLE_COMPFIX='true'
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
    unset IPYTHON
fi

# Add color to ls
export LS_OPTIONS="--color=auto"

LS_COLORS='rs=0:di=01;94:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:*.py=93:*.ipynb=93';

# LS_COLORS="ow=01;90:di=01;90"

if [[ "$(uname)" == "Darwin" ]]; then
    export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd
fi
export LS_COLORS

# Add packages to the path if they exists.
local_directories=('anaconda2' 'anaconda3' 'tools/arcanist' '.local/bin')
for directory in $local_directories;
do
    if [[ -d "$HOME/$directory" ]]; then
        export PATH="$HOME/$directory/bin:$PATH";
    fi
done

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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/mangesh/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/home/mangesh/anaconda3/etc/profile.d/conda.sh" ]; then
#         . "/home/mangesh/anaconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/home/mangesh/anaconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<


if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
    source /etc/profile.d/vte.sh
fi
