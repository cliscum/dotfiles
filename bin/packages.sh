#!/bin/sh

set -eux

exec pacman -Syu --needed \
  base-devel \
  bc \
  chromium \
  docker \
  dunst \
  emacs \
  git \
  gnome-nettool \
  gnome-power-manager \
  gnome-system-monitor \
  go \
  gsimplecal \
  htop \
  i3-wm \
  i3blocks \
  i3lock \
  i3status \
  intel-ucode \
  keepassx2 \
  lsof \
  nodejs \
  pacman-contrib \
  pavucontrol \
  perl-html-parser \
  playerctl \
  pulseaudio \
  pulseaudio-alsa \
  python3 \
  redshift \
  rofi \
  sudo \
  termite \
  tmux \
  tree \
  vim \
  xautolock \
  xorg \
  xorg-xinit \
  xorg-xrandr \
  zsh
