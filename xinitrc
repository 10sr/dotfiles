setxkbmap -model "acer_laptop" -layout "jp" -option "ctrl:nocaps" # this line must comes first
xmodmap -e 'keycode 135 = Alt_R Meta_R' # menu key as alt
xmodmap -e 'keycode 101 = Alt_R Meta_R' # hiragana key as alt
#xmodmap -e 'remove Lock = Caps_Lock'
#xmodmap -e 'add Control = Caps_Lock'

type synclient >/dev/null 2>&1 && {
    synclient VertEdgeScroll=0
    synclient HorizEdgeScroll=0
    synclient MaxTapTime=0 
    synclient MaxSpeed=2.0
    synclient MinSpeed=0.2
}

xset s 1800 # go blank after 30 min
xset dpms 1810 1820 1830        # standby, suspend and off seem to be not different for my environment

unset LC_MESSAGES
export LANG=ja_JP.utf8
export BROWSER=firefox

