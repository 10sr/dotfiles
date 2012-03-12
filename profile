#!/bin/sh

# export PS1="\$ "
export LANG=C # ja_JP.UTF-8
export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export BROWSER=firefox
export ENV=~/.shrc
# export TMP=/tmp
# export TEMP=/tmp
test -f "${HOME}/.pythonrc" && export PYTHONSTARTUP="${HOME}/.pythonrc"
export PYTHONPATH=~/.py

addtopath(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || PATH="$p:${PATH}"
    done
}
# export PATH="${PATH}:${HOME}/bin"
addtopath ${HOME}/bin

type setterm >/dev/null 2>&1 && setterm -blank 30 -powersave on # -powerdown 10
# in my environment powerdown does not work

env > $HOME/.env.txt
