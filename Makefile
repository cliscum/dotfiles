SHELL = /bin/zsh --extendedglob

all: install link clean ssm

install:
	git submodule update --init --recursive

update:
	git submodule update --remote

# Symlink all from here to home.
link:
	cp -asf $$(pwd)/^(README.md|Makefile|.git|*.ssm)(D) ~/

# Clean dangling links in home pointing here.
clean:
	find -L ~/ -type l -lname "$$(pwd)/*" -delete -print || echo 'clean failures'

SSM := \
	.Xresources \
	.config/i3blocks/config \
	.config/termite/config \
	.gitconfig \
	.wakatime.cfg \
	.xinitrc \
	.xprofile \
	.xsettingsd \
	bin/i3-init

SSMGEN = $(SSM:%=$(HOME)/%)

ssm: $(SSMGEN)

$(SSMGEN): $(HOME)/%: %.ssm $(HOME)/bin/ssm-replace
	@mkdir -p $(dir $@)
	$(HOME)/bin/ssm-replace $< $@
