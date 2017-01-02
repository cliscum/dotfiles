#!/bin/zsh

set -ex

exec 2> ~/.xsession-errors

export DE=gnome
export DESKTOP_SESSION=gnome
export BROWSER=xdg-open
export QT_QPA_PLATFORMTHEME=qgnomeplatform
export GDK_SCALE=1
export GDK_DPI_SCALE=1

if [ -r /etc/default/locale ]; then
  source /etc/default/locale
  export LANG LANGUAGE
fi

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && source "$f"
  done
  unset f
fi

if [ -f ~/.Xresources ]; then
  xrdb -merge -I"$HOME" ~/.Xresources
fi

if [ -f ~/.xprofile ]; then
  source ~/.xprofile
fi

exec i3
