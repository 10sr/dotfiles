#!/bin/sh

type fortune >/dev/null 2>&1 && {
    echo
    fortune
    echo
    fortune -o
    echo
}

# export PS1="\$ "
# export LANG=ja_JP.UTF-8
export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export ENV=~/.shrc
# export TMP=/tmp
# export TEMP=/tmp
test -f "${HOME}/.pythonrc" && export PYTHONSTARTUP="${HOME}/.pythonrc"
export PYTHONPATH="~/.local/share/lib/python3.2/site-packages"

__add_to_path(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || PATH="$p:${PATH}"
    done
}
# export PATH="${PATH}:${HOME}/bin"
__add_to_path ${HOME}/bin

type setterm >/dev/null 2>&1 && setterm -blank 30 -powersave on # -powerdown 10
# in my environment powerdown does not work

mkdir -p ~/.backup/log
mkdir -p /tmp/$USER-tmp

