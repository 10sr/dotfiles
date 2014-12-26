#!/bin/sh
set -xe

dir="`dirname $0`"
top="$dir"/..

# setup.sh
sh "$top"/setup.sh help

# shrc
cat <<__EOC__ | sh -si
. "$top"/shrc || exit 1
__EOC__

# emacs
emacs --version
emacs -q --debug-init --batch --eval "(setq debug-on-error t)" --load "$top"/emacs.el --kill
