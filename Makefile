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

tests = test_el test_sh
test: $(tests)

test_shs = test_shrc test_profile
test_sh: $(test_shs)

setups = setup_darwin setup_directories setup_emacs
setup: $(setups)

setup_darwins = setup_darwin_defaults setup_darwin_daemon
setup_darwin: $(setup_darwins)

setup_directories = $(localdir) $(vardir) $(bindir)
setup_directory = $(setup_directories)

.PHONY: all default test $(tests) $(test_shs) \
	setup $(setups) $(setup_darwins) emacs

all: default



# setups
# ======

# create directories
# ------------------
$(localdir) $(vardir) $(bindir):
	mkdir -vp $@




# darwin
# ------

setup_darwin_defaults:
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

setup_darwin_daemon:
	test "`launchctl getenv LC_ALL`" = C || sudo launchctl setenv LC_ALL C
	if ! (launchctl list | grep com.apple.locate) >/dev/null ;\
	then \
		sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist ;\
	fi



# emacs setup
# -----------

setup_emacs: emacs.el
	$(emacs) -q --debug-init --batch --load $< -f my-auto-install-package


# test
# ====

test_profile: profile
	sh -exc 'for sh in $(shrc_loadables); do $$sh -n $<; done'

test_shrc: shrc
	sh -exc 'for sh in $(shrc_loadables); do $$sh -n $<; done'


test_el: emacs.el
	$(emacs) -q --debug-init --batch --eval "(setq debug-on-error t)" \
		--load $< --kill
