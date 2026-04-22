#!/usr/bin/zsh
#
# Single-file zsh configuration — replaces oh-my-zsh
#

[[ -f /usr/facebook/ops/rc/master.zshrc ]] && source /usr/facebook/ops/rc/master.zshrc

# Dumb terminal: give a bare prompt and bail (helps emacs tramp, etc.)
if [[ "$TERM" == "dumb" ]]; then
  unsetopt zle prompt_cr prompt_subst
  if whence -w precmd >/dev/null; then unfunction precmd; fi
  if whence -w preexec >/dev/null; then unfunction preexec; fi
  PS1='$ '
  return
fi

# ── Environment ─────────────────────────────────────────────────────
export DOTFILES=$HOME/dotfiles
export ZSH_HOME=~/
export IPYTHON=1
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=xterm-256color
export PYTHONIOENCODING='utf-8'

typeset -U path PATH  # force unique entries in $PATH

local hostname=${(%):-%m}

# Dotfiles auto-update check
export UPDATE_DOTFILES_DAYS=7
export DISABLE_UPDATE_PROMPT="true"
env DOTFILES=$DOTFILES DISABLE_UPDATE_PROMPT=$DISABLE_UPDATE_PROMPT zsh -f $DOTFILES/utils/check_for_upgrade.sh

# ── History ──────────────────────────────────────────────────────────
HISTFILE="${HISTFILE:-$HOME/.zsh_history}"
HISTSIZE=50000
SAVEHIST=10000

setopt extended_history        # record timestamps
setopt hist_expire_dups_first  # delete dups first when full
setopt hist_ignore_dups        # ignore consecutive duplicates
setopt hist_ignore_space       # ignore space-prefixed commands
setopt hist_verify             # show expansion before running
setopt share_history           # share history across sessions

function omz_history {
  local clear list stamp REPLY
  zparseopts -E -D c=clear l=list f=stamp E=stamp i=stamp t:=stamp
  if [[ -n "$clear" ]]; then
    print -nu2 "This action will irreversibly delete your command history. Are you sure? [y/N] "
    builtin read -E
    [[ "$REPLY" = [yY] ]] || return 0
    print -nu2 >| "$HISTFILE"
    fc -p "$HISTFILE"
    print -u2 History file deleted.
  elif [[ $# -eq 0 ]]; then
    builtin fc "${stamp[@]}" -l 1
  else
    builtin fc "${stamp[@]}" -l "$@"
  fi
}
alias history='omz_history'

# ── Directory navigation ─────────────────────────────────────────────
setopt auto_cd            # type a dir name to cd into it
setopt auto_pushd         # cd pushes onto the directory stack
setopt pushd_ignore_dups  # no duplicates in the stack
setopt pushdminus         # swap +/- meaning for pushd

alias -- -='cd -'
alias 1='cd -1'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

alias md='mkdir -p'
alias rd=rmdir

function d () {
  if [[ -n $1 ]]; then
    dirs "$@"
  else
    dirs -v | head -n 10
  fi
}

# ── Misc options ─────────────────────────────────────────────────────
setopt multios              # redirect to multiple streams
setopt long_list_jobs       # verbose job notifications
setopt interactivecomments  # allow # comments interactively

WORDCHARS=''  # only alphanumeric chars are word characters

# ── Completion ───────────────────────────────────────────────────────
zmodload -i zsh/complist
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit

unsetopt menu_complete   # don't autoselect the first entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on successive tab
setopt complete_in_word
setopt always_to_end

# Case-insensitive, partial-word and substring completion
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:*:*:*:*' menu select
bindkey -M menuselect '^o' accept-and-infer-next-history

# Complete . and .. special directories
zstyle ':completion:*' special-dirs true

zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USERNAME -o pid,user,comm -w -w"

# Disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Completion caching
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/compcache"

# Don't complete system users
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
  clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
  gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
  ldap lp mail mailman mailnull man messagebus mldonkey mysql nagios \
  named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
  operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
  usbmux uucp vcsa wwwrun xfs '_*'

zstyle '*' single-ignored show

# SSH host completion
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_hosts}(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# Directory stack completion
compdef _dirs d

# ── Key bindings ─────────────────────────────────────────────────────
# Enable application mode for correct terminfo values
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init()   { echoti smkx }
  function zle-line-finish() { echoti rmkx }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

bindkey -e  # emacs mode

# Up/Down: prefix-based history search
autoload -U up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey "^[[A" up-line-or-beginning-search    # Up
bindkey "^[[B" down-line-or-beginning-search  # Down
if [[ -n "${terminfo[kcuu1]}" ]]; then bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search; fi
if [[ -n "${terminfo[kcud1]}" ]]; then bindkey "${terminfo[kcud1]}" down-line-or-beginning-search; fi

# Page Up / Page Down
if [[ -n "${terminfo[kpp]}" ]]; then bindkey "${terminfo[kpp]}" up-line-or-history; fi
if [[ -n "${terminfo[knp]}" ]]; then bindkey "${terminfo[knp]}" down-line-or-history; fi

# Home / End
if [[ -n "${terminfo[khome]}" ]]; then bindkey "${terminfo[khome]}" beginning-of-line; fi
if [[ -n "${terminfo[kend]}" ]];  then bindkey "${terminfo[kend]}"  end-of-line; fi

# Shift-Tab: reverse menu completion
if [[ -n "${terminfo[kcbt]}" ]]; then bindkey "${terminfo[kcbt]}" reverse-menu-complete; fi

# Backspace / Delete
bindkey '^?' backward-delete-char
if [[ -n "${terminfo[kdch1]}" ]]; then
  bindkey "${terminfo[kdch1]}" delete-char
else
  bindkey "^[[3~" delete-char
fi

bindkey '^[[3;5~' kill-word         # Ctrl-Delete
bindkey '^[[1;5C' forward-word      # Ctrl-Right
bindkey '^[[1;5D' backward-word     # Ctrl-Left
bindkey '\ew' kill-region            # Esc-w
bindkey -s '\el' '^qls\n'           # Esc-l: run ls
bindkey '^r' history-incremental-search-backward
bindkey ' ' magic-space              # Space: expand history without executing

# Edit command in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

bindkey "^[m" copy-prev-shell-word   # Alt-m: copy previous word

# ── Colors and appearance ────────────────────────────────────────────
autoload -U colors && colors
setopt prompt_subst

# 256-color support
typeset -AHg FX FG BG
FX=(
  reset     "%{[00m%}"
  bold      "%{[01m%}" no-bold      "%{[22m%}"
  dim       "%{[02m%}" no-dim       "%{[22m%}"
  italic    "%{[03m%}" no-italic    "%{[23m%}"
  underline "%{[04m%}" no-underline "%{[24m%}"
  blink     "%{[05m%}" no-blink     "%{[25m%}"
  reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)
for color in {000..255}; do
  FG[$color]="%{[38;5;${color}m%}"
  BG[$color]="%{[48;5;${color}m%}"
done

function spectrum_ls() {
  setopt localoptions nopromptsubst
  local ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}
  for code in {000..255}; do
    print -P -- "$code: ${FG[$code]}${ZSH_SPECTRUM_TEXT}%{$reset_color%}"
  done
}

# ls coloring
export LSCOLORS="Gxfxcxdxbxegedabagacad"
if [[ -z "$LS_COLORS" ]]; then
  if (( $+commands[dircolors] )); then
    [[ -f "$HOME/.dircolors" ]] && source <(dircolors -b "$HOME/.dircolors") || source <(dircolors -b)
  else
    export LS_COLORS="di=1;36:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43"
  fi
fi

# Colored ls (macOS: -G, Linux: --color)
case "$OSTYPE" in
  (darwin|freebsd)*) command ls -G /dev/null &>/dev/null && alias ls='ls -G' ;;
  *)                 command ls --color /dev/null &>/dev/null && alias ls='ls --color=tty' ;;
esac

# Colored diff
if command diff --color /dev/null{,} &>/dev/null; then
  function diff { command diff --color "$@" }
fi

# Colored grep
alias grep="grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.venv,venv}"
alias egrep="grep -E"
alias fgrep="grep -F"

# ── Async prompt infrastructure ──────────────────────────────────────
zmodload zsh/system
autoload -Uz is-at-least add-zsh-hook

typeset -ga _omz_async_functions
typeset -gA _OMZ_ASYNC_FDS _OMZ_ASYNC_PIDS _OMZ_ASYNC_OUTPUT

function _omz_register_handler {
  setopt localoptions noksharrays unset
  if [[ -z "$1" ]] || (( ! ${+functions[$1]} )) || (( ${_omz_async_functions[(Ie)$1]} )); then
    return
  fi
  _omz_async_functions+=("$1")
  if (( ! ${precmd_functions[(Ie)_omz_async_request]} )) && (( ${+functions[_omz_async_request]} )); then
    add-zsh-hook precmd _omz_async_request
  fi
}

function _omz_async_request {
  setopt localoptions noksharrays unset
  local -i ret=$?
  local handler
  for handler in ${_omz_async_functions}; do
    (( ${+functions[$handler]} )) || continue
    local fd=${_OMZ_ASYNC_FDS[$handler]:--1}
    local pid=${_OMZ_ASYNC_PIDS[$handler]:--1}
    if (( fd != -1 && pid != -1 )) && { true <&$fd } 2>/dev/null; then
      exec {fd}<&-
      zle -F $fd
      if [[ -o MONITOR ]]; then kill -TERM -$pid 2>/dev/null; else kill -TERM $pid 2>/dev/null; fi
    fi
    _OMZ_ASYNC_FDS[$handler]=-1
    _OMZ_ASYNC_PIDS[$handler]=-1
    exec {fd}< <(
      builtin echo ${sysparams[pid]}
      () { return $ret }
      $handler
    )
    _OMZ_ASYNC_FDS[$handler]=$fd
    is-at-least 5.8 || command true
    read -u $fd "_OMZ_ASYNC_PIDS[$handler]"
    zle -F "$fd" _omz_async_callback
  done
}

function _omz_async_callback() {
  emulate -L zsh
  local fd=$1 err=$2
  if [[ -z "$err" || "$err" == "hup" ]]; then
    local handler="${(k)_OMZ_ASYNC_FDS[(r)$fd]}"
    local old_output="${_OMZ_ASYNC_OUTPUT[$handler]}"
    IFS= read -r -u $fd -d '' "_OMZ_ASYNC_OUTPUT[$handler]"
    if [[ "$old_output" != "${_OMZ_ASYNC_OUTPUT[$handler]}" ]]; then
      zle .reset-prompt
      zle -R
    fi
    exec {fd}<&-
  fi
  zle -F "$fd"
  _OMZ_ASYNC_FDS[$handler]=-1
  _OMZ_ASYNC_PIDS[$handler]=-1
}

add-zsh-hook precmd _omz_async_request

# ── Git prompt functions ─────────────────────────────────────────────
DISABLE_UNTRACKED_FILES_DIRTY="true"

function __git_prompt_git() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

function _omz_git_prompt_info() {
  if ! __git_prompt_git rev-parse --git-dir &>/dev/null; then return 0; fi
  local ref
  ref=$(__git_prompt_git symbolic-ref --short HEAD 2>/dev/null) \
    || ref=$(__git_prompt_git describe --tags --exact-match HEAD 2>/dev/null) \
    || ref=$(__git_prompt_git rev-parse --short HEAD 2>/dev/null) \
    || return 0
  echo "${ZSH_THEME_GIT_PROMPT_PREFIX}${ref:gs/%/%%}$(parse_git_dirty)${ZSH_THEME_GIT_PROMPT_SUFFIX}"
}

function parse_git_dirty() {
  local STATUS FLAGS=('--porcelain')
  if [[ "${DISABLE_UNTRACKED_FILES_DIRTY:-}" == "true" ]]; then
    FLAGS+='--untracked-files=no'
  fi
  FLAGS+="--ignore-submodules=${GIT_STATUS_IGNORE_SUBMODULES:-dirty}"
  STATUS=$(__git_prompt_git status ${FLAGS} 2>/dev/null | tail -n 1)
  if [[ -n $STATUS ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
    echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}

function git_current_branch() {
  local ref
  ref=$(__git_prompt_git symbolic-ref --quiet HEAD 2>/dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return
    ref=$(__git_prompt_git rev-parse --short HEAD 2>/dev/null) || return
  fi
  echo ${ref#refs/heads/}
}

# Async wrappers: prompt reads from async output
function git_prompt_info() {
  [[ -n "${_OMZ_ASYNC_OUTPUT[_omz_git_prompt_info]}" ]] && echo -n "${_OMZ_ASYNC_OUTPUT[_omz_git_prompt_info]}"
}

function _defer_async_git_register() {
  _omz_register_handler _omz_git_prompt_info
  add-zsh-hook -d precmd _defer_async_git_register
  unset -f _defer_async_git_register
}
precmd_functions=(_defer_async_git_register $precmd_functions)

# ── Prompt (mangesh theme) ───────────────────────────────────────────
local_machines=("PAC02LC18LFFT3" "PA-MBP-C02LC18LFFT3" "garfield" "PAC02C10WXMD6P")
local machine_name=''
if [[ ${local_machines[(r)$hostname]} != $hostname ]]; then
  local machine_name="%m:"
fi

special_users=("root" "hadoop")
local user_prefix=""
if [[ ${special_users[(r)$USERNAME]} = $USERNAME ]]; then
  local user_prefix="%{$fg[red]%}%n@%{$reset_color%}"
fi

PROMPT='$user_prefix%{$fg[green]%}$machine_name%{$fg[cyan]%}%3~ %{$fg_bold[blue]%}$(git_prompt_info)%{$fg[cyan]%}%% %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

# ── Terminal title ───────────────────────────────────────────────────
function title {
  setopt localoptions nopromptsubst
  [[ -n "${INSIDE_EMACS:-}" && "$INSIDE_EMACS" != vterm ]] && return
  : ${2=$1}
  case "$TERM" in
    cygwin|xterm*|putty*|rxvt*|konsole*|ansi|mlterm*|alacritty*|st*|foot*|contour*|wezterm*)
      print -Pn "\e]2;${2:q}\a"
      print -Pn "\e]1;${1:q}\a"
      ;;
    screen*|tmux*)
      print -Pn "\ek${1:q}\e\\"
      ;;
    *)
      if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
        print -Pn "\e]2;${2:q}\a"
        print -Pn "\e]1;${1:q}\a"
      elif (( ${+terminfo[fsl]} && ${+terminfo[tsl]} )); then
        print -Pn "${terminfo[tsl]}$1${terminfo[fsl]}"
      fi
      ;;
  esac
}

ZSH_THEME_TERM_TAB_TITLE_IDLE="%15<..<%~%<<"
ZSH_THEME_TERM_TITLE_IDLE="%n@%m:%~"
[[ "$TERM_PROGRAM" == Apple_Terminal ]] && ZSH_THEME_TERM_TITLE_IDLE="%n@%m"

function _termsupport_precmd {
  title "$ZSH_THEME_TERM_TAB_TITLE_IDLE" "$ZSH_THEME_TERM_TITLE_IDLE"
}

function _termsupport_preexec {
  emulate -L zsh
  setopt extended_glob
  local -a cmdargs=("${(z)2}")
  if [[ "${cmdargs[1]}" = fg ]]; then
    local job_id jobspec="${cmdargs[2]#%}"
    case "$jobspec" in
      <->)    job_id=${jobspec} ;;
      ""|%|+) job_id=${(k)jobstates[(r)*:+:*]} ;;
      -)      job_id=${(k)jobstates[(r)*:-:*]} ;;
      [?]*)   job_id=${(k)jobtexts[(r)*${(Q)jobspec}*]} ;;
      *)      job_id=${(k)jobtexts[(r)${(Q)jobspec}*]} ;;
    esac
    if [[ -n "${jobtexts[$job_id]}" ]]; then
      1="${jobtexts[$job_id]}"
      2="${jobtexts[$job_id]}"
    fi
  fi
  local CMD="${1[(wr)^(*=*|sudo|ssh|mosh|rake|-*)]:gs/%/%%}"
  local LINE="${2:gs/%/%%}"
  title "$CMD" "%100>...>${LINE}%<<"
}

if [[ -z "$INSIDE_EMACS" || "$INSIDE_EMACS" = vterm ]]; then
  add-zsh-hook precmd _termsupport_precmd
  # Uncomment to show running command name in tab title:
  # add-zsh-hook preexec _termsupport_preexec
fi

# OSC 7: report cwd to terminal (enables "new tab in same directory")
zmodload zsh/langinfo

function _urlencode() {
  emulate -L zsh
  setopt norematchpcre
  local -a opts
  zparseopts -D -E -a opts r m P
  local in_str="$@" url_str="" spaces_as_plus
  [[ -z $opts[(r)-P] ]] && spaces_as_plus=1
  local str="$in_str"
  local encoding=$langinfo[CODESET]
  local safe_encodings=(UTF-8 utf8 US-ASCII)
  if [[ -z ${safe_encodings[(r)$encoding]} ]]; then
    str=$(echo -E "$str" | iconv -f $encoding -t UTF-8) || return 1
  fi
  local i byte ord LC_ALL=C
  export LC_ALL
  local reserved=';/?:@&=+$,'
  local mark='_.!~*''()-'
  local dont_escape="[A-Za-z0-9"
  [[ -z $opts[(r)-r] ]] && dont_escape+=$reserved
  [[ -z $opts[(r)-m] ]] && dont_escape+=$mark
  dont_escape+="]"
  for (( i = 1; i <= ${#str}; ++i )); do
    byte="$str[i]"
    if [[ "$byte" =~ "$dont_escape" ]]; then
      url_str+="$byte"
    elif [[ "$byte" == " " && -n $spaces_as_plus ]]; then
      url_str+="+"
    else
      ord=$(( [##16] #byte ))
      url_str+="%$ord"
    fi
  done
  echo -E "$url_str"
}

if [[ -z "$INSIDE_EMACS" && -z "$SSH_CLIENT" && -z "$SSH_TTY" ]]; then
  case "$TERM" in
    xterm*|putty*|rxvt*|konsole*|mlterm*|alacritty*|screen*|tmux*|contour*|foot*) ;;
    *) case "$TERM_PROGRAM" in Apple_Terminal|iTerm.app) ;; *) false ;; esac ;;
  esac && {
    function _termsupport_cwd {
      setopt localoptions unset
      local URL_HOST URL_PATH
      URL_HOST="$(_urlencode -P $HOST)" || return 1
      URL_PATH="$(_urlencode -P $PWD)" || return 1
      printf "\e]7;file://%s%s\e\\" "${URL_HOST}" "${URL_PATH}"
    }
    add-zsh-hook precmd _termsupport_cwd
  }
fi

# ── Paste safety ─────────────────────────────────────────────────────
autoload -Uz bracketed-paste-magic url-quote-magic
zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

# ── VCS info security patch (CVE-2021-45444) ─────────────────────────
autoload -Uz +X regexp-replace VCS_INFO_formats 2>/dev/null && {
  typeset _PATCH='for tmp (base base-name branch misc revision subdir) hook_com[$tmp]="${hook_com[$tmp]//\%/%%}"'
  typeset _PATCH_ID=vcs_info-patch-9b9840f2-91e5-4471-af84-9e9a0dc68c1b
  if [[ "$functions[VCS_INFO_formats]" != *$_PATCH_ID* ]]; then
    regexp-replace 'functions[VCS_INFO_formats]' \
      "VCS_INFO_hook 'post-backend'" \
      ': ${_PATCH_ID}; ${_PATCH}; ${MATCH}'
  fi
  unset _PATCH _PATCH_ID
}

# ── Clipboard ────────────────────────────────────────────────────────
function clipcopy() { cat "${1:-/dev/stdin}" | pbcopy; }
function clippaste() { pbpaste; }

# ── Utility functions ────────────────────────────────────────────────
function zsh_stats() {
  fc -l 1 \
    | awk '{ CMD[$2]++; count++; } END { for (a in CMD) print CMD[a] " " CMD[a]*100/count "% " a }' \
    | grep -v "./" | sort -nr | head -n 20 | column -c3 -s " " -t | nl
}

function mkcd takedir() { mkdir -p $@ && cd ${@:$#} }

function take() {
  if [[ $1 =~ ^(https?|ftp).*\.(tar\.(gz|bz2|xz)|tgz)$ ]]; then
    local data="$(mktemp)"; curl -L "$1" > "$data"; tar xf "$data"
    cd "$(tar tf "$data" | head -n 1)"; rm "$data"
  elif [[ $1 =~ ^(https?|ftp).*\.(zip)$ ]]; then
    local data="$(mktemp)"; curl -L "$1" > "$data"; unzip "$data" -d "./"
    cd "$(unzip -l "$data" | awk 'NR==4 {print $4}' | sed 's/\/.*//')"; rm "$data"
  elif [[ $1 =~ ^([A-Za-z0-9]\+@|https?|git|ssh|ftps?|rsync).*\.git/?$ ]]; then
    git clone "$1"; cd "$(basename ${1%%.git})"
  else
    takedir "$@"
  fi
}

alias _='sudo '

path() {
  echo $PATH | tr ":" "\n" | \
    awk "{ sub(\"/usr\",   \"$fg_no_bold[green]/usr$reset_color\"); \
           sub(\"/bin\",   \"$fg_no_bold[cyan]/bin$reset_color\"); \
           sub(\"/opt\",   \"$fg_no_bold[blue]/opt$reset_color\"); \
           sub(\"/sbin\",  \"$fg_no_bold[magenta]/sbin$reset_color\"); \
           sub(\"/local\", \"$fg_no_bold[yellow]/local$reset_color\"); \
           print }"
}

json_pretty_print() {
  python -m json.tool $1 | pygmentize -l json | less
}

function bm() {
  let "index = $HISTCMD - 1"
  echo $history[$index] " # $@" >> ~/.bookmarks.zsh
}

function histogram() {
  awk '{print $1}' | sort | uniq -c | sort -rn | head -50 | awk '!max{max=$1;}{r="";i=s=60*$1/max;while(i-->0)r=r"#";printf "%15s %5d %s %s",$2,$1,r,"\n";}'
}

# Calculator
autoload -U zcalc
function __calc_plugin { zcalc -e "$@" }
alias calc='noglob __calc_plugin'

# ── Git functions & aliases ──────────────────────────────────────────
autoload -Uz is-at-least
git_version="${${(As: :)$(git version 2>/dev/null)}[3]}"

function git_main_branch() {
  command git rev-parse --git-dir &>/dev/null || return
  local ref
  for ref in refs/{heads,remotes/{origin,upstream}}/{main,trunk,mainline,default,stable,master}; do
    if command git show-ref -q --verify $ref; then echo ${ref:t}; return 0; fi
  done
  echo master; return 1
}

function git_develop_branch() {
  command git rev-parse --git-dir &>/dev/null || return
  local branch
  for branch in dev devel develop development; do
    if command git show-ref -q --verify refs/heads/$branch; then echo $branch; return 0; fi
  done
  echo develop; return 1
}

function grename() {
  if [[ -z "$1" || -z "$2" ]]; then echo "Usage: $0 old_branch new_branch"; return 1; fi
  git branch -m "$1" "$2"
  if git push origin :"$1"; then git push --set-upstream origin "$2"; fi
}

function work_in_progress() {
  command git -c log.showSignature=false log -n 1 2>/dev/null | grep -q -- "--wip--" && echo "WIP!!"
}

function ggu() {
  local b; [[ $# != 1 ]] && b="$(git_current_branch)"
  git pull --rebase origin "${b:-$1}"
}
compdef _git ggu=git-pull

function ggl() {
  if [[ $# != 0 ]] && [[ $# != 1 ]]; then git pull origin "${*}"
  else local b; [[ $# == 0 ]] && b="$(git_current_branch)"; git pull origin "${b:-$1}"; fi
}
compdef _git ggl=git-pull

function ggp() {
  if [[ $# != 0 ]] && [[ $# != 1 ]]; then git push origin "${*}"
  else local b; [[ $# == 0 ]] && b="$(git_current_branch)"; git push origin "${b:-$1}"; fi
}
compdef _git ggp=git-push

unset git_version

# ── History aliases ──────────────────────────────────────────────────
alias h='history'
alias hl='history | less'
alias hs='history | grep'
alias hsi='history | grep -i'

# ── Sudo toggle (Esc-Esc) ───────────────────────────────────────────
__sudo-replace-buffer() {
  local old=$1 new=$2 space=${2:+ }
  if [[ $CURSOR -le ${#old} ]]; then
    BUFFER="${new}${space}${BUFFER#$old }"
    CURSOR=${#new}
  else
    LBUFFER="${new}${space}${LBUFFER#$old }"
  fi
}

sudo-command-line() {
  [[ -z $BUFFER ]] && LBUFFER="$(fc -ln -1)"
  local WHITESPACE=""
  if [[ ${LBUFFER:0:1} = " " ]]; then
    WHITESPACE=" "
    LBUFFER="${LBUFFER:1}"
  fi
  {
    local EDITOR=${SUDO_EDITOR:-${VISUAL:-$EDITOR}}
    if [[ -z "$EDITOR" ]]; then
      case "$BUFFER" in
        sudo\ -e\ *) __sudo-replace-buffer "sudo -e" "" ;;
        sudo\ *)     __sudo-replace-buffer "sudo" "" ;;
        *)           LBUFFER="sudo $LBUFFER" ;;
      esac
      return
    fi
    local cmd="${${(Az)BUFFER}[1]}"
    local realcmd="${${(Az)aliases[$cmd]}[1]:-$cmd}"
    local editorcmd="${${(Az)EDITOR}[1]}"
    if [[ "$realcmd" = (\$EDITOR|$editorcmd|${editorcmd:c}) \
      || "${realcmd:c}" = ($editorcmd|${editorcmd:c}) ]] \
      || builtin which -a "$realcmd" | command grep -Fx -q "$editorcmd"; then
      __sudo-replace-buffer "$cmd" "sudo -e"
      return
    fi
    case "$BUFFER" in
      $editorcmd\ *) __sudo-replace-buffer "$editorcmd" "sudo -e" ;;
      \$EDITOR\ *)   __sudo-replace-buffer '$EDITOR' "sudo -e" ;;
      sudo\ -e\ *)   __sudo-replace-buffer "sudo -e" "$EDITOR" ;;
      sudo\ *)       __sudo-replace-buffer "sudo" "" ;;
      *)             LBUFFER="sudo $LBUFFER" ;;
    esac
  } always {
    LBUFFER="${WHITESPACE}${LBUFFER}"
    zle && zle redisplay
  }
}

zle -N sudo-command-line
bindkey '\e\e' sudo-command-line

if [[ -n "$TMUX" || -n "$SSH_CONNECTION" ]]; then
    precmd() { print -Pn "\e]1;⟦%m⟧ %~\a" }
fi

# ── Dircycle (Ctrl+Shift+Arrows) ─────────────────────────────────────
switch-to-dir() {
  if [[ $1 == -- ]]; then
    [[ -d "$2" ]] && builtin pushd -q "$2" &>/dev/null
    return $?
  fi
  setopt localoptions nopushdminus
  [[ ${#dirstack} -eq 0 ]] && return 1
  while ! builtin pushd -q $1 &>/dev/null; do
    builtin popd -q $1
    [[ ${#dirstack} -eq 0 ]] && return 1
  done
}

insert-cycledleft() {
  switch-to-dir +1 || return $?
  local fn; for fn in chpwd $chpwd_functions precmd $precmd_functions; do (( $+functions[$fn] )) && $fn; done
  zle reset-prompt
}
zle -N insert-cycledleft

insert-cycledright() {
  switch-to-dir -0 || return $?
  local fn; for fn in chpwd $chpwd_functions precmd $precmd_functions; do (( $+functions[$fn] )) && $fn; done
  zle reset-prompt
}
zle -N insert-cycledright

insert-cycledup() {
  switch-to-dir -- .. || return $?
  local fn; for fn in chpwd $chpwd_functions precmd $precmd_functions; do (( $+functions[$fn] )) && $fn; done
  zle reset-prompt
}
zle -N insert-cycledup

insert-cycleddown() {
  switch-to-dir -- "$(find . -mindepth 1 -maxdepth 1 -type d | sort -n | head -n 1)" || return $?
  local fn; for fn in chpwd $chpwd_functions precmd $precmd_functions; do (( $+functions[$fn] )) && $fn; done
  zle reset-prompt
}
zle -N insert-cycleddown

bindkey "\e[1;6D" insert-cycledleft   # Ctrl+Shift+Left
bindkey "\e[1;6C" insert-cycledright  # Ctrl+Shift+Right
bindkey "\e[1;6A" insert-cycledup     # Ctrl+Shift+Up
bindkey "\e[1;6B" insert-cycleddown   # Ctrl+Shift+Down

# ── Mark/jump completion ─────────────────────────────────────────────
function _completemarks {
  reply=($(ls $MARKPATH))
}
compctl -K _completemarks jump
compctl -K _completemarks unmark

# ── Machine-specific PATH ────────────────────────────────────────────
local_machines=("starlight")

if [[ ${local_machines[(r)$hostname]} == $hostname ]]; then
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

  local_directories=('.local' '.npm-global')
  for directory in $local_directories; do
    if [[ -d "$HOME/$directory" ]]; then
      path+=("$HOME/$directory/bin")
    fi
  done

  if [[ -f "${HOME}/.iterm2_shell_integration.zsh" ]]; then
    source "${HOME}/.iterm2_shell_integration.zsh"
  else
    print -P "%F{yellow}⚠ iTerm2 shell integration not found%f"
  fi
fi

# ── Specific Servers ────────────────────────────────────────────
specific_servers=("devvm8210")
if [[ ${specific_servers[(r)$hostname]} == $hostname ]]; then
    # myclaw instance: wyld
    alias myclaw-wyld='MYCLAW_HOME=~/.myclaw-wyld myclaw'

    # myclaw instance: poc
    alias myclaw-poc='MYCLAW_HOME=~/.myclaw-poc myclaw'
fi


specific_servers=("mangesh-mbp")
if [[ ${specific_servers[(r)$hostname]} == $hostname ]]; then
    # PARA workspace configuration
    export PARA_MODE="local"
    export PARA_ROOT="$HOME/Library/CloudStorage/GoogleDrive-mangesh@meta.com/My Drive/claude"
fi

# ── Less configuration ──────────────────────────────────────────────
LESSPIPE=$(command -v src-hilite-lesspipe.sh)
if [[ -n "$LESSPIPE" ]]; then
  export LESSOPEN="| ${LESSPIPE} %s"
fi
if type lesspipe.sh >/dev/null 2>&1; then
  export LESSOPEN='|lesspipe.sh %s'
fi
if type pygmentize >/dev/null 2>&1; then
  export LESSCOLORIZER='pygmentize'
fi
export LESS='--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --RAW-CONTROL-CHARS --HILITE-UNREAD --tabs=4 --no-init --window=-4'

# ── User aliases (overrides git aliases like gl, gp, gs) ─────────────
if [[ -f $ZSH_HOME/.aliases.sh ]]; then
  source $ZSH_HOME/.aliases.sh
else
  print -P "%F{yellow}⚠ aliases not found: $ZSH_HOME/.aliases.sh%f"
fi

# ── Tilix / VTE ──────────────────────────────────────────────────────
if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  source /etc/profile.d/vte.sh
fi

# ── Conda ────────────────────────────────────────────────────────────
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

# ── NVM ──────────────────────────────────────────────────────────────
export NVM_DIR="$HOME/.nvm"
if [ -s "$NVM_DIR/nvm.sh" ]; then
  \. "$NVM_DIR/nvm.sh"
else
  print -P "%F{yellow}⚠ nvm not found: $NVM_DIR/nvm.sh%f"
fi
if [ -s "$NVM_DIR/bash_completion" ]; then
  \. "$NVM_DIR/bash_completion"
else
  print -P "%F{yellow}⚠ nvm bash_completion not found: $NVM_DIR/bash_completion%f"
fi

# ── Local env ────────────────────────────────────────────────────────
if [ -f "$HOME/.local/bin/env" ]; then
  . "$HOME/.local/bin/env"
else
  print -P "%F{yellow}⚠ local env not found: $HOME/.local/bin/env%f"
fi

# ── Plugins (must be near end, order matters) ────────────────────────
# Syntax highlighting
if [[ -f $DOTFILES/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source $DOTFILES/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
else
  print -P "%F{yellow}⚠ plugin not found: zsh-syntax-highlighting%f"
fi

# History substring search (must be after syntax highlighting)
# Type a partial command, then Up/Down to cycle matches containing that substring
if [[ -f $DOTFILES/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh ]]; then
  source $DOTFILES/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
  [[ -n "${terminfo[kcuu1]}" ]] && bindkey "${terminfo[kcuu1]}" history-substring-search-up
  [[ -n "${terminfo[kcud1]}" ]] && bindkey "${terminfo[kcud1]}" history-substring-search-down
else
  print -P "%F{yellow}⚠ plugin not found: zsh-history-substring-search%f"
fi

# Multi-word history search on Ctrl-R
# Type multiple words to filter history to entries containing all of them
if [[ -f $DOTFILES/zsh/plugins/history-search-multi-word/history-search-multi-word.plugin.zsh ]]; then
  source $DOTFILES/zsh/plugins/history-search-multi-word/history-search-multi-word.plugin.zsh
else
  print -P "%F{yellow}⚠ plugin not found: history-search-multi-word%f"
fi

# Claude Config
alias cl="claude --model 'claude-opus-4-7[1m]'"
