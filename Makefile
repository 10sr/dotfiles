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

tests = test-el test-sh
test: $(tests)

setups = setup-darwin setup-directories setup-emacs
setup: $(setups)

.PHONY: all default test $(tests) setup $(setups)




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

test_shs = test-shrc test-profile test-setup.sh test-xinitrc test-xprograms
test-sh: $(test_shs)
.PHONY: $(test_shs)

$(test_shs): test-%: %
	sh -ec 'for sh in $(shrc_loadables); do $$sh -n $<; done'

test-el: emacs.el
	EMACS_EL_DRY_RUN=t $(emacs) -q --debug-init --batch \
		--eval "(setq debug-on-error t)" --load $< --kill
