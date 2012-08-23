#!/bin/sh

# ~/.dotfiles/profile

# export PS1="\$ "
export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export ENV=~/.shrc
export PYTHONDOCS=/usr/share/doc/python/html/
test -f "${HOME}/.pythonrc" && export PYTHONSTARTUP="${HOME}/.pythonrc"
#export PYTHONPATH="~/.local/share/lib/python3.2/site-packages"

__add_to_path(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || PATH="${PATH}:$p"
    done
}
# export PATH="${PATH}:${HOME}/bin"
__add_to_path ${HOME}/.local/bin /c/mingw/bin /c/mingw/msys/1.0/bin

# it is not so good http://archive.linux.or.jp/JF/JFdocs/Program-Library-HOWTO/shared-libraries.html
export LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"

type setterm >/dev/null 2>&1 && setterm -blank 30 -powersave on # -powerdown 10
# in my environment powerdown does not work

export TMP="/tmp/${USER}-tmp"
export TEMP="$TMP"
mkdir -p "$TMP"
