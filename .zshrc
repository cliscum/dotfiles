# `.zshrc' is sourced in interactive shells. It should contain commands to set
# up aliases, functions, options, key bindings, etc.

# This is mainly for emacs remote sessions, and there is probably a better way.
if [ "$TERM" = 'dumb' ]; then
  unsetopt zle
  PS1='$ '
  return
fi

fpath=(~/.zsh $fpath)

autoload -U compinit && compinit
autoload -U colors && colors
autoload -U select-word-style && select-word-style B # bash +subword
autoload -Uz add-zsh-hook

zmodload  -i zsh/complist

# Colorize completion lists
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} 'ma=00;30;103'

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Turn on menu selection when at least 4 completion matches
zstyle ':completion:*' menu select=4

# Kill completion PIDs red
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

bindkey -e # emacs
bindkey -M menuselect '^o' accept-and-infer-next-history
bindkey "^[[1;3C" forward-word  # [M-Right]
bindkey "^[[1;3D" backward-word # [M-Left]
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

setopt AUTO_PUSHD
setopt COMBINING_CHARS
setopt COMPLETE_IN_WORD
setopt CORRECT
setopt EXTENDED_GLOB
setopt EXTENDED_HISTORY
setopt HASH_LIST_ALL
setopt HIST_IGNORE_DUPS
setopt INC_APPEND_HISTORY
setopt INTERACTIVE_COMMENTS
setopt NO_CASE_GLOB
setopt PROMPT_SUBST

source $HOME/.zsh/zsh-git-prompt/zshrc.sh
source $HOME/.zsh/z/z.sh

ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg[cyan]%}"
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[red]%}%{●%G%}"
ZSH_THEME_GIT_PROMPT_CONFLICTS="%{$fg[red]%}%{✖%G%}"
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}%{✚%G%}"
ZSH_THEME_GIT_PROMPT_BEHIND="%{↓%G%}"
ZSH_THEME_GIT_PROMPT_AHEAD="%{↑%G%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{…%G%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}%{✔%G%}"

prompt_color=magenta
if [ "$HOST" = "strudel" ]; then
  prompt_color=red
fi
export PROMPT="%m%b %~ \$(git_super_status)
%{\$fg_bold[${prompt_color}]%}>%{\$reset_color%} "

uname -a |grep -q '^Linux' && alias ls='ls --color=auto'
uname -a |grep -q '^Darwin' && export CLICOLOR=1

alias grep='grep --color'
alias less='less -R'
alias vi=vim
alias zgrep='zgrep --color'

tput_capabilities() {
  capabilities=$(IFS=$'\n'; echo "$*")
  tput -S <<-EOF
$capabilities
EOF
}

tput init
export LESS_TERMCAP_mb=$(tput_capabilities 'setaf 1' bold)  # blink
export LESS_TERMCAP_md=$(tput_capabilities 'setaf 4' bold)  # bold
export LESS_TERMCAP_me=$(tput_capabilities sgr0)            # end
export LESS_TERMCAP_se=$(tput_capabilities sgr0)            # end standout
export LESS_TERMCAP_so=$(tput_capabilities 'setab 11' bold) # standout
export LESS_TERMCAP_ue=$(tput_capabilities sgr0)            # end underline
export LESS_TERMCAP_us=$(tput_capabilities 'setaf 3' smul)  # underline

# Sets the terminal or terminal multiplexer window title.
function set-window-title {
  local title_format{,ted}
  zstyle -s ':prezto:module:terminal:window-title' format 'title_format' || title_format="%s"
  zformat -f title_formatted "$title_format" "s:$argv"

  if [[ "$TERM" == screen* ]]; then
    title_format="\ek%s\e\\"
  else
    title_format="\e]2;%s\a"
  fi

  printf "$title_format" "${(V%)title_formatted}"
}

# Terminal window title settings lifted from:
# https://github.com/sorin-ionescu/prezto/blob/ca03fd/modules/terminal/init.zsh

# Sets the tab and window titles with a given command.
function _terminal-set-titles-with-command {
  emulate -L zsh
  setopt EXTENDED_GLOB

  # Get the command name that is under job control.
  if [[ "${2[(w)1]}" == (fg|%*)(\;|) ]]; then
    # Get the job name, and, if missing, set it to the default %+.
    local job_name="${${2[(wr)%*(\;|)]}:-%+}"

    # Make a local copy for use in the subshell.
    local -A jobtexts_from_parent_shell
    jobtexts_from_parent_shell=(${(kv)jobtexts})

    jobs "$job_name" 2>/dev/null > >(
      read index discarded
      # The index is already surrounded by brackets: [1].
      _terminal-set-titles-with-command "${(e):-\$jobtexts_from_parent_shell$index}"
    )
  else
    # Set the command name, or in the case of sudo or ssh, the next command.
    local cmd="${${2[(wr)^(*=*|sudo|ssh|-*)]}:t}"
    unset MATCH

    set-window-title " $cmd"
  fi
}

# Sets the tab and window titles with a given path.
function _terminal-set-titles-with-path {
  emulate -L zsh
  setopt EXTENDED_GLOB

  local absolute_path="${${1:a}:-$PWD}"
  local abbreviated_path="${absolute_path/#$HOME/~}"
  unset MATCH

  set-window-title "$abbreviated_path"
}

# Sets the tab and window titles before the prompt is displayed.
add-zsh-hook precmd _terminal-set-titles-with-path

# Sets the tab and window titles before command execution.
add-zsh-hook preexec _terminal-set-titles-with-command
