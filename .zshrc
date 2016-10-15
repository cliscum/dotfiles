# This must come before zprezto modules are loaded.
autoload -U select-word-style && select-word-style B # bash +subword

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export EDITOR='emacsclient -t'
export GTK2_RC_FILES="$HOME/.gtkrc.mine"
export HISTSIZE=50000 # >= SAVEHIST
export PYTHONDONTWRITEBYTECODE=1
export SAVEHIST=50000 # <= HISTSIZE
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export TERMINAL=termite
export VISUAL=emacsclient

# This is mainly for emacs remote sessions, and there is probably a better way.
# if [ "$TERM" = 'dumb' ]; then
#   unsetopt zle
#   PS1='$ '
#   return
# fi

fpath=(~/.zsh $fpath)

zmodload -i zsh/complist

# bindkey -e # emacs
bindkey -M menuselect '^o' accept-and-infer-next-history
bindkey "^[[1;3C" forward-word  # [M-Right]
bindkey "^[[1;3D" backward-word # [M-Left]
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

uname -a |grep -q '^Linux' && alias ls='ls --color=auto'
uname -a |grep -q '^Darwin' && export CLICOLOR=1

alias grep='grep --color'
alias less='less -R'
alias vi=vim
alias zgrep='zgrep --color'

# From https://wiki.archlinux.org/index.php/zsh
# Normally, the path should be set in ~/.zshenv, but Arch Linux sources
# /etc/profile after sourcing ~/.zshenv.
typeset -U path
path=($HOME/bin $HOME/.local/bin $path[@])
