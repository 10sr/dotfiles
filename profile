#!/bin/sh

# ~/.dotfiles/profile

test -n "$DOTFILES_PROFILE" && return
export DOTFILES_PROFILE=t

# export PS1="\$ "
export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export ENV=~/.shrc

mkdir -p $HOME/.local/lib/python/site-packages
export PYTHONDOCS=/usr/share/doc/python/html/
export PYTHONPATH="${PYTHONPATH}:\
${HOME}/my/bin/py:${HOME}/.local/lib/python/site-packages"
test -f "${HOME}/.dotfiles/rc.py" && \
    export PYTHONSTARTUP="${HOME}/.dotfiles/rc.py"
#export PYTHONPATH="~/.local/share/lib/python3.2/site-packages"

__add_to_path(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || \
            PATH="${PATH}:$p"
    done
}
# export PATH="${PATH}:${HOME}/bin"
__add_to_path ${HOME}/.local/bin /c/mingw/bin /c/mingw/msys/1.0/bin

# it is not so good
# http://archive.linux.or.jp/JF/JFdocs/Program-Library-HOWTO/shared-libraries.html
export LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"

type setterm >/dev/null 2>&1 && setterm -blank 30 -powersave on # -powerdown 10
# in my environment powerdown does not work

if test -n "$TMPDIR"
then
    export TMP=$TMPDIR
else
    export TMP=/tmp/
fi
export TMP="${TMP}${USER}-tmp"
export TEMP="$TMP"
mkdir -p "$TMP"

echo .dotfiles/profile processed.
