emacs ?= emacs

shrc_loadables = sh bash zsh

.PHONY: all default test emacs

all: default

emacs: emacs.el
	$(emacs) -q --debug-init --batch --load $< -f my-auto-install-package


test: test_el test_shrc

test_shrc: shrc
	sh -exc 'for sh in $(shrc_loadables); do $$sh -n $<; done'

test_el: emacs.el
	$(emacs) -q --debug-init --batch --eval "(setq debug-on-error t)" \
		--load $< --kill
