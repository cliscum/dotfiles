#!/bin/sh

set -eux

pacman -Syu --needed \
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
  pactl \
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
  xrandr \
  xsettings \
  zsh
