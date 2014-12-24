#!/bin/sh
set -xe

dir="`dirname $0`"
top="$dir"/..

# setup.sh
sh "$top"/setup.sh help

# shrc
for sh in sh bash zsh
do
    sh -n "$top"/shrc
done



# emacs
emacs --version
emacs -q --debug-init --batch --eval "(setq debug-on-error t)" --load "$top"/emacs.el --kill
