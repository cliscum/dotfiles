# Dotfiles

Install submodules.

    git submodules update --init --recursive

Symlink this repository's contents to home. (Assumes `zsh`.)

    cp -asf $(pwd)/^README.md*(D) ~/

Clean up dangling symlinks that point into this repo.

    find -L ~/ -type l -lname "$(pwd)/*"
