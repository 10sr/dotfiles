#!/bin/sh
env > $HOME/.env.txt

# export PS1="\$ "
export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export BROWSER=firefox
export ENV=~/.shrc
# export TMP=/tmp
# export TEMP=/tmp

addtopath(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || PATH="$p:${PATH}"
    done
}
# export PATH="${PATH}:${HOME}/bin"
addtopath ${HOME}/bin

test -f "${HOME}/.pythonrc" && export PYTHONSTARTUP="${HOME}/.pythonrc"
export PYTHONPATH=~/.py

