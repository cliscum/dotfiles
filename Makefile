SHELL = /bin/zsh --extendedglob

all: install link clean

install:
	git submodule update --init --recursive

update:
	git submodule update --remote --recursive

# Symlink all from here to home.
link:
	cp -asf $$(pwd)/^(README.md|Makefile|.git)(D) ~/

# Clean dangling links in home pointing here.
clean:
	find -L ~/ -type l -lname "$$(pwd)/*" -delete -print
