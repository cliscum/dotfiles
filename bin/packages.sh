#!/bin/sh
#
# Package installations to help setting up new Arch systems.

set -eux

pacman -Syu --needed \
  aws-cli \
  base-devel \
  bc \
  chromium \
  curl \
  docker \
  dunst \
  emacs \
  git \
  git-review \
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
  iotop \
  keepassx2 \
  lsof \
  nodejs \
  pacman-contrib \
  pavucontrol \
  perl-html-parser \
  playerctl \
  pulseaudio \
  pulseaudio-alsa \
  python-gobject \
  python-pip \
  python3 \
  redshift \
  rofi \
  sudo \
  termite \
  the_silver_searcher \
  tmux \
  tree \
  vim \
  wget \
  xautolock \
  xorg \
  xorg-xinit \
  xorg-xrandr \
  zsh

pip install wakatime
