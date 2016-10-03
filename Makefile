SHELL = /bin/zsh --extendedglob

all: install link clean

install:
	git submodule update --init --recursive

update:
	git submodule update --remote --recursive

# Symlink all from here to home.
link:
	cp -asf $$(pwd)/^(README.md|Makefile)*(D) ~/

# Clean dangling links.
clean:
	find -L ~/ -type l -lname "$$(pwd)/*" -delete -print
