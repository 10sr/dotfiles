# Makefile --- 10sr make dotfiles
# 2014, 10sr. Unlicensed <http://unlicense.org>

# Variable definitions

home ?= $(HOME)

dotfiles_dir ?= $(home)/10sr_dotfiles
dotfiles_url_base = https://raw.githubusercontent.com/10sr/dotfiles/master/
dotfiles_git = git@github.com:10sr/dotfiles.git
dotfiles_git_pub = http://github.com/10sr/dotfiles.git

localdir = $(home)/.local
vardir = $(home)/.var
bindir = $(localdir)/bin

current = $(shell date)
ostype = $(shell uname)

shrc_loadables = sh bash zsh
shrc_common_tpl =

emacs ?= $(shell which emacs 2>/dev/null)
git ?= $(shell which git 2>/dev/null)
curl ?= $(shell which curl 2>/dev/null)

# Targets

all: default

tests = test-el
test: test-syntax $(tests)

test_syntaxes = test-syntax-el test-syntax-sh
test-syntax: $(test_syntaxes)

setups = setup-darwin setup-directories setup-emacs setup-gitconf \
	setup-repository setup-util setup-tmux
setup: $(setups)



# `make check` is just an alias for `make test`
check: test

# Similarly, check-syntax is test-syntax
check-syntax: test-syntax

.PHONY: all default \
	test check $(tests) \
	test-syntax check-syntax $(test_syntaxes)\
	setup $(setups)





# setups
# ======



# setup repository
# ----------------

setup-repository:
ifeq (,$(git))
	false "Git not installed"
endif
	if ssh git@github.com 2>&1 | grep 'successfully authenticated'; \
	then \
		echo "Using $(dotfiles_git)"; \
		$(git) clone $(dotfiles_git) $(dotfiles_dir); \
	else \
		echo "Using $(dotfiles_git_pub)"; \
		$(git) clone $(dotfiles_git_pub) $(dotfiles_dir); \
	fi



# utils
# -----

setup_utils = colortable16.sh 256colors2.pl pacapt ack-2.12
setup-util: $(setup_utils)
.PHONY: $(setup_utils)

setup_utils_path = $(setup_utils:%=$(bindir)/%)

$(setup_utils): %: $(bindir)/%

$(setup_utils_path):
	$(curl) -L --url "$(util_url)" --output "$@"
	chmod +x "$@"

colortable16.sh: \
	util_url = https://gist.github.com/10sr/6852317/raw/colortable16.sh
256colors2.pl: util_url = https://gist.github.com/10sr/6852331/raw/256colors2.pl
pacapt: util_url = https://github.com/icy/pacapt/raw/ng/pacapt
ack-2.12: util_url = http://beyondgrep.com/ack-2.12-single-file



# create directories
# ------------------

setup_directories = $(localdir) $(vardir) $(bindir)
setup-directory: $(setup_directories)

$(localdir) $(vardir) $(bindir):
	mkdir -vp $@



# darwin setup
# ------------

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



# git config setup
# ----------------

ifneq (,$(git))
git_conf = $(git) config --global
endif

xz = $(shell which xz 2>/dev/null)

setup-gitconf:
ifeq (,$(git))
$(warnning "Git program not found")
else
	$(git_conf) user.name '10sr'
	$(git_conf) user.email '8slashes+git@gmail.com'

	$(git_conf) core.autocrlf false
	$(git_conf) core.excludesfile '~/.gitignore'
	$(git_conf) color.ui auto
	$(git_conf) status.relativePaths false
	$(git_conf) status.showUntrackedFiles normal
	$(git_conf) log.date iso
	$(git_conf) push.default current
ifneq (,$(xz))
	$(git_conf) tar.txz.command "xz -c"
endif
	$(git_conf) alias.graph "log --graph --date-order -C -M --pretty=tformat:\"%C(green)%h%C(reset) %C(white)%ad%C(reset) %C(red)%an%C(reset)%C(yellow)%d%C(reset) %C(white bold)%s%C(reset)\" --date=short -n 499"
	$(git_conf) alias.st "status -s -b"
	$(git_conf) alias.b "branch"
	$(git_conf) alias.sb "show-branch"
	$(git_conf) alias.ci "commit --verbose"
	$(git_conf) alias.co "checkout"
	$(git_conf) alias.cim "commit --verbose -m"
	$(git_conf) alias.di "diff --color"
	$(git_conf) alias.me "merge --no-ff --stat --verbose"
	$(git_conf) alias.ffme "merge --ff-only --stat --verbose"
	$(git_conf) alias.gr "grep -n"
	$(git_conf) alias.ls "ls-files"
	# $(git_conf) alias.ls "ls-files -v --full-name"
	# $(git_conf) alias.ls "status -u -s ."
	$(git_conf) alias.sl "!sl"
	# $(git_conf) alias.my-ls "ls-files | xargs ls"
	# $(git_conf) alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
	$(git_conf) alias.addi "add -i"
	# Add patch to index
	$(git_conf) alias.ap "apply --cached"
	$(git_conf) alias.clean-p "diff --quiet"
	$(git_conf) alias.echo-ref "for-each-ref --format='%(refname:short)'"

	$(git_conf) alias.todo "grep -nH -E -i 'todo:|note:|fixme:'"

	$(git_conf) alias.snap '! gitdir="`git rev-parse --git-dir`" && : >>"$gitdir"/logs/refs/snapshot && cmt=`git stash create` && test -n "$cmt" && git update-ref refs/snapshot $cmt && echo Snapshot created: $cmt'

	#$(git_conf) alias.wc "!git ls-files -z | xargs -0 wc"
	# $(git_conf) push.default "simple"
ifneq (,$(iswindows))
	$(git_conf) core.fileMode false
endif
endif



# tmux setup
# ----------

setup-tmux:





# test
# ====

test_els = test-el-emacs.el
test-el: $(test_els)
.PHONY: $(test_els)

$(test_els): test-el-%: %
	$(emacs) -Q -batch -f batch-byte-compile $<
	EMACS_EL_DRY_RUN=t $(emacs) -q --debug-init --batch \
		--eval "(setq debug-on-error t)" --load $<c --kill





# test syntax
# ===========

test_syntax_shs = test-syntax-shrc test-syntax-profile \
	test-syntax-xinitrc test-syntax-xprograms
test-syntax-sh: $(test_syntax_shs)
.PHONY: $(test_syntax_shs)

$(test_syntax_shs): test-syntax-%: %
	sh -ec 'for sh in $(shrc_loadables); do $$sh -n $<; done'



test_syntax_els = test-syntax-emacs.el
test-syntax-el: $(test_syntax_els)
.PHONY: $(test_syntax_els)

sexp_elisp_syntax_check = \
	(with-temp-buffer \
		(emacs-lisp-mode) \
		(insert-file-contents file) \
		(condition-case err \
			(check-parens) \
			(user-error \
				(error (format "%s:%d:%d:Unmatched brancet or quote" \
								file \
								(line-number-at-pos) \
								(- (point) (point-at-bol)))))))

$(test_syntax_els): test-syntax-%: %
	$(emacs) -Q --debug-init --batch \
		--eval '(let ((file "$<")) $(sexp_elisp_syntax_check))' --kill
