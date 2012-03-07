xmodmap -e 'keycode 135 = Alt_R Meta_R' # menu key as alt
xmodmap -e 'keycode 101 = Alt_R Meta_R' # hiragana key as alt
#xmodmap -e 'remove Lock = Caps_Lock'
#xmodmap -e 'add Control = Caps_Lock'
setxkbmap -model "acer_laptop" -layout "jp" -option "ctrl:nocaps"

type synclient >/dev/null 2&>1 && {
    synclient VertEdgeScroll=0
    synclient HorizEdgeScroll=0
    synclient MaxTapTime=0 
    synclient MaxSpeed=0.4
    synclient MinSpeed=0.2
}

test -f "$HOME/.fehbg" &&
type feh >/dev/null 2>&1 &&
sh "$HOME/.fehbg"

#export LC_MESSAGES=C
LANG=ja_JP.utf8

