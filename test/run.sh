#!/bin/sh
set -xe

dir="`dirname $0`"
top="$dir"/..

# shrc
cat <<__EOC__ | sh -si
. "$top"/shrc || exit 1
__EOC__

# setup.sh
sh "$top"/setup.sh help

# emacs
emacs -q --batch --load "$top"/emacs.el --kill
