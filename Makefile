home ?= $(HOME)

dotfiles_dir ?= $(home)/10sr_dotfiles
dotfiles_url_base=https://raw.githubusercontent.com/10sr/dotfiles/master/

localdir = $(home)/.local
vardir = $(home)/.var
bindir = $(localdir)/bin

current = $(shell date)
ostype = $(shell uname)

shrc_loadables = sh bash zsh
shrc_common_tpl =

emacs ?= emacs

all: default

# `make check` is just an alias for `make test`
check: test

# Similarly, check-syntax is test-syntax
check-syntax: test-syntax

tests = test-el
test: $(tests) test-syntax

test_syntaxes = test-syntax-el test-syntax-sh
test-syntax: $(test_syntaxes)

setups = setup-darwin setup-directories setup-emacs
setup: $(setups)

.PHONY: all default \
	test check $(tests) \
	test-syntax check-syntax $(test_syntaxes)\
	setup $(setups)




# setups
# ======

# create directories
# ------------------

setup_directories = $(localdir) $(vardir) $(bindir)
setup-directory: $(setup_directories)

$(localdir) $(vardir) $(bindir):
	mkdir -vp $@

# darwin
# ------

setup_darwins = setup-darwin-defaults setup-darwin-daemon
setup-darwin: $(setup_darwins)
.PHONY: $(setup_darwins)

setup-darwin-defaults:
# http://appdrill.net/60641/mac-boot-mute.html
#sudo nvram SystemAudioVolume=%80

# add quit entry in menu
	defaults write com.apple.finder QuitMenuItem -bool YES
# show full path on titlebar
	defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
# do not show desktop icons
	defaults write com.apple.finder CreateDesktop -boolean false

	killall Finder

# disable dashboard
#defaults write com.apple.dashboard mcx-disabled -bool YES

setup-darwin-daemon:
	test "`launchctl getenv LC_ALL`" = C || sudo launchctl setenv LC_ALL C
	if ! (launchctl list | grep com.apple.locate) >/dev/null ;\
	then \
		sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist ;\
	fi

# emacs setup
# -----------

setup-emacs: emacs.el
	$(emacs) -q --debug-init --batch --load $< -f my-auto-install-package




# test
# ====

test-el: emacs.el
	EMACS_EL_DRY_RUN=t $(emacs) -q --debug-init --batch \
		--eval "(setq debug-on-error t)" --load $< --kill



# test syntax
# ===========

test_syntax_shs = test-syntax-shrc test-syntax-profile test-syntax-setup.sh \
	test-syntax-xinitrc test-syntax-xprograms
test-syntax-sh: $(test_syntax_shs)
.PHONY: $(test_syntax_shs)

$(test_syntax_shs): test-syntax-%: %
	sh -ec 'for sh in $(shrc_loadables); do $$sh -n $<; done'
