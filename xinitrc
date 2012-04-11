test "`hostname`" == "arch-aspireone" &&
setxkbmap -model "acer_laptop" -layout "jp" # this line must comes first

setxkbmap -option "ctrl:nocaps"
xmodmap -e 'keycode 135 = Alt_R Meta_R' # menu key as alt
xmodmap -e 'keycode 101 = Alt_R Meta_R' # hiragana key as alt
#xmodmap -e 'remove Lock = Caps_Lock'
#xmodmap -e 'add Control = Caps_Lock'

type synclient >/dev/null 2>&1 && {
    synclient MaxSpeed=1.6
    synclient MinSpeed=0.2
    synclient TouchpadOff=2     # tapping and scrolling are disabled
}

xset s 1800 # go blank after 30 min
xset dpms 1810 1820 1830        # standby, suspend and off seem not to be different for my environment

unset LC_MESSAGES
export LANG=ja_JP.utf8
export BROWSER=firefox

__upper(){
    echo $1 | tr '[:lower:]' '[:upper:]'
}

XDG_DIRS=$HOME/.xdg-dirs
# XDG_DESKTOP_DIR=$XDG_DIRS/Desktop
# XDG_DOWNLOAD_DIR=$XDG_DIRS/Download
# XDG_TEMPLATES_DIR=$XDG_DIRS/Templates
# XDG_PUBLICSHARE_DIR=$XDG_DIRS/Public
# XDG_DOCUMENTS_DIR=$XDG_DIRS/Document
# XDG_MUSIC_DIR=$XDG_DIRS/Music
# XDG_PICTURES_DIR=$XDG_DIRS/Pictures
# XDG_VIDEOS_DIR=$XDG_DIRS/Videos

for f in Desktop Download Templates Publicshare Documents Music Pictures Videos
do
    export XDG_`__upper $f`_DIR=$XDG_DIRS/$f
    mkdir -p $XDG_DIRS/$f
done
#echo $XDG_DESKTOP_DIR
