# `.zprofile' is similar to `.zlogin', except that it is sourced before
# `.zshrc'. `.zprofile' is meant as an alternative to `.zlogin' for ksh fans;
# the two are not intended to be used together, although this could certainly be
# done if desired.

# From https://wiki.archlinux.org/index.php/zsh
# Normally, the path should be set in ~/.zshenv, but Arch Linux sources
# /etc/profile after sourcing ~/.zshenv. To prevent your $PATH being
# overwritten, set it in ~/.zprofile.
typeset -U path
path=($HOME/bin $HOME/.local/bin $path[@])
