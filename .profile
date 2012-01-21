#!/bin/sh
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

    test -f "$HOME/.fehbg" &&
    type feh >/dev/null 2>&1 &&
    sh "$HOME/.fehbg"
else
    export LANG=C
fi

# export PS1="\$ "
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
# export PATH="${PATH}:${HOME}/bin"
addtopath ${HOME}/bin

test -f "${HOME}/.pythonrc" && export PYTHONSTARTUP="${HOME}/.pythonrc"
export PYTHONPATH=~/.py

