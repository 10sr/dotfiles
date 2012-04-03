test "`hostname`" == "arch-aspireone" &&
setxkbmap -model "acer_laptop" -layout "jp" # this line must comes first

setxkbmap -option "ctrl:nocaps"
xmodmap -e 'keycode 135 = Alt_R Meta_R' # menu key as alt
xmodmap -e 'keycode 101 = Alt_R Meta_R' # hiragana key as alt
#xmodmap -e 'remove Lock = Caps_Lock'
#xmodmap -e 'add Control = Caps_Lock'

type synclient >/dev/null 2>&1 && {
    synclient VertEdgeScroll=0
    synclient HorizEdgeScroll=0
    synclient MaxTapTime=0      # disable tap
    synclient MaxSpeed=2.0
    synclient MinSpeed=0.2
    synclient VertTwoFingerScroll=0
    synclient HorizTwoFingerScroll=0

}

xset s 1800 # go blank after 30 min
xset dpms 1810 1820 1830        # standby, suspend and off seem to be not different for my environment

unset LC_MESSAGES
export LANG=ja_JP.utf8
export BROWSER=firefox

