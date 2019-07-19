#!/bin/zsh
#
# Package installations to help setting up new Arch systems.
#
# Usage: packages.sh [+[+]] [package] ...

set -eu

arg=${1:-}
if [ "$arg" -a "x${arg[1,1]}" = 'x+' ]; then
  shift
fi

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
  jdk-openjdk \
  jdk8-openjdk \
  keepassxc \
  lsof \
  nodejs \
  noto-fonts \
  pacman-contrib \
  parallel \
  pavucontrol \
  perl-html-parser \
  playerctl \
  postgresql \
  pulseaudio \
  pulseaudio-alsa \
  python-gobject \
  python-pip \
  python3 \
  redshift \
  rofi \
  scala \
  sshuttle \
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
  ttf-inconsolata \
  ttf-liberation \
  ttf-opensans \
  ttf-roboto \
  ttf-ubuntu-font-family \
  unzip \
  vim \
  wget \
  xautolock \
  xorg \
  xorg-xinit \
  xorg-xrandr \
  zsh \
  $*

if [ -x $HOME/bin/aurtool ]; then
  if [ "$arg" -a "x${arg[1,1]}" = 'x+' ]; then
    $HOME/bin/aurtool \
      dropbox \
      jsonnet \
      signal \
      spotify \
      ttf-merriweather \
      ttf-merriweather-sans \
      ttf-oswald \
      ttf-quintessential \
      ttf-signika \
      wakatime
  fi

  # These don't keep PKGBUILD up to date w/ version, so they always install.
  if [ "x$arg" = 'x++' ]; then
    $HOME/bin/aurtool \
      wlroots-git \
      sway-git \
      ttf-google-fonts-git
  fi
fi
