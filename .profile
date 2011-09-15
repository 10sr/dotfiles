#!/bin/bash
if [ -n "${DESKTOP_SESSION}" ]; then
    xmodmap -e 'keycode 135 = Alt_R Meta_R' # menu key as alt
    xmodmap -e 'keycode 101 = Alt_R Meta_R' # hiragana key as alt
    xmodmap -e 'remove Lock = Caps_Lock'
    xmodmap -e 'add Control = Caps_Lock'

    synclient VertEdgeScroll=0
    synclient HorizEdgeScroll=0
    synclient MaxTapTime=0 
    synclient MaxSpeed=0.4
    synclient MinSpeed=0.2
fi

export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export BROWSER=firefox
# export TMP=/tmp
# export TEMP=/tmp

addtopath(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || PATH="$p:${PATH}"
    done
}
# export PATH="${PATH}:${HOME}/bin:${HOME}/dbx/dev/bin"
addtopath ${HOME}/bin

type git 1>/dev/null 2>&1 && test ! -f ~/.gitconfig && {
    # export GISTY_DIR="$HOME/dev/gists"
    git config --global user.name "10sr"
    git config --global user.email sr10@users.sourceforge.jp
    git config --global core.autocrlf false
    # git config --global github.token **
}

test -f "${HOME}/.pythonrc" && export PYTHONSTARTUP="${HOME}/.pythonrc"
export PYTHONPATH=~/.py

