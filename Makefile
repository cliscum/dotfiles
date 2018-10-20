SHELL := /bin/zsh --extendedglob

SSM := \
	.Xresources \
	.config/i3blocks/config \
	.config/systemd/user/xautolock.service \
	.config/termite/config \
	.gitconfig \
	.wakatime.cfg \
	.xinitrc \
	.xprofile \
	.xsettingsd \
	bin/i3-init

SSMGEN := $(SSM:%=$(HOME)/%)
SSM_REPLACE := $(HOME)/bin/ssm-replace

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

ssm: $(SSMGEN)

$(SSMGEN): $(HOME)/%: %.ssm $(SSM_REPLACE)
	@mkdir -p $(dir $@)
	$(SSM_REPLACE) $< $@
