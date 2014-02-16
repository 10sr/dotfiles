#!/bin/sh

test -n "$DISPLAY" || return
test -z "$ROXTERM_PID" || return
test -z "$SSH_CONNECTION" || return

# if already in x return
case "$TERM" in
    xterm*) return;;
esac

unset TMUX

setxkbmap -layout "jp"
# use localectl list-x11-keymap-models to get list of available models
test "`hostname`" = "arch-aspireone" && \
    setxkbmap -model "acer_laptop" -layout "jp" # this line must comes first
test "`hostname`" = "arch-mba" && \
    setxkbmap -model "apple_laptop" -layout "jp" # this line must comes first

setxkbmap -option "ctrl:nocaps,ctrl:menu_rctrl"
# use `xev` to get keycode
xmodmap -e 'keycode 101 = Alt_R Meta_R' # hiragana key as alt
#xmodmap -e 'remove Lock = Caps_Lock'
#xmodmap -e 'add Control = Caps_Lock'
#xmodmap -e 'keycode 134 = Pointer_Button3

type synclient >/dev/null 2>&1 && {
    synclient MaxSpeed=1.6
    synclient MinSpeed=0.2
    synclient TouchpadOff=2     # tapping and scrolling are disabled
}

xset s 1800 # go blank after 30 min
# standby, suspend and off seem not to be different for my environment
xset dpms 1810 1820 1830

unset LC_MESSAGES
export LANG=ja_JP.utf8
export BROWSER=firefox
export DMENU_FONT='-*-dejavu sans mono-*-r-*-*-11-*-*-*-*-*-*-*'

__upper(){
    echo $1 | tr '[:lower:]' '[:upper:]'
}

XDG_DIRS=$HOME/.xdg-dirs

for f in Desktop Download Templates Publicshare Documents Music Pictures Videos
do
    export XDG_`__upper $f`_DIR=$XDG_DIRS/$f
    mkdir -p $XDG_DIRS/$f
done

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi
