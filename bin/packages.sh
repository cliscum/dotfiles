#!/bin/sh
#
# Package installations to help setting up new Arch systems.

set -eux

sudo pacman -Syu --needed \
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
  noto-fonts \
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
  ttf-bitstream-vera \
  ttf-croscore \
  ttf-dejavu \
  ttf-droid \
  ttf-fira-mono \
  ttf-fira-sans \
  ttf-freefont \
  ttf-inconsolata \
  ttf-liberation \
  ttf-opensans \
  ttf-roboto \
  ttf-ubuntu-font-family \
  vim \
  wget \
  xautolock \
  xorg \
  xorg-xinit \
  xorg-xrandr \
  zsh

if [ -x $HOME/bin/aurtool ]; then
  $HOME/bin/aurtool \
    dropbox \
    jsonnet \
    spotify \
    ttf-merriweather \
    ttf-merriweather-sans \
    ttf-oswald \
    ttf-quintessential \
    ttf-signika \
    wakatime
  # These don't keep PKGBUILD up to date w/ version, so they always install.
  # $HOME/bin/aurtool \
  #   sway-git \
  #   ttf-google-fonts-git \
fi
