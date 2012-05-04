#!/bin/bash

##########################
# system type

alias ismsys=false
alias iscygwin=false
alias iswindows=false
alias isdarwin=false
alias islinux=false

case `uname` in
    (MINGW*) alias ismsys=true ;;
    (CYGWIN*) alias iscygwin=true ;;
    (Darwin*) alias isdarwin=true ;;
    (Linux*) alias islinux=true ;;
esac

( ismsys || iscygwin ) && alias iswindows=true

##########################################
null(){
    "$@" >/dev/null 2>&1
}
__try_exec(){
    type $1 >/dev/null 2>&1 && "$@"
}

export PS1="\$(__my_prompt_function)\$ "
# PROMPT_COMMAND=prompt_function
if false iswindows
then
    export PAGER='tr -d \\r | less'
else
    export PAGER="less"
fi
export LESS="-iRMX"

if null type vim
then
    export EDITOR=vim
else
    export EDITOR=vi
fi
export LC_MESSAGES=C
# export CDPATH=".:~"
export VISUAL="$EDITOR"
export GIT_PAGER="$PAGER"
export GIT_EDITOR="$EDITOR"

null type stty && {
    stty stop undef        # unbind C-s to stop displaying output
    # stty erase '^h'
}

if iswindows; then
    # export TMP=/tmp
    # export TEMP=/tmp
    # export PS1=" \[\e[32m\]\u@\H \[\e[33m\]\w\[\e[0m\] \d \t\n\s \# \j \$ "
    # export PS1=" [\[\e[33m\]\w\[\e[0m\]]\n\[\e[32m\]\u@\H\[\e[0m\] \d \t \s.\v\nhist:\# jobs:\j \$ "
    export USER=$USERNAME
fi

#######################

uname -a
echo TERM $TERM connected to $(tty), running $BASH $BASH_VERSION
echo

###################################
# some aliases and functions

iswindows || test "$TERM" == dumb || _coloroption=" --color=always"

alias ls="ls -hCF --time-style=long-iso${_coloroption}"
iswindows && alias ls="ls -hCF"
# export GREP_OPTIONS=""
alias grep="grep -n${_coloroption}"
# alias ll="ls -l"
# alias la="ls -A"
# alias lla="ls -Al"
# alias less=""
# alias vl=/usr/share/vim/vimcurrent/macros/less.sh
alias em="emacs -nw"
null type vim && alias vi=vim
alias pstree="LANG=C pstree"
alias cp="cp -v"
alias mv="mv -v"
alias psall="ps auxww"
alias q=exit
alias p="$PAGER"
alias c=cat
alias e3=e3em
alias dirs="dirs -v -l | \grep -v \$(printf '%s$' \$PWD)"
alias po=popd
alias pu=pushd
alias sudo="sudo "              # use aliases through sudo
alias halt="sudo halt"
alias reboot="sudo reboot"
alias suspend="dbus-send --system --print-reply --dest=org.freedesktop.UPower \
    /org/freedesktop/UPower org.freedesktop.UPower.Suspend"
alias hibernate="dbus-send --system --print-reply --dest=org.freedesktop.UPower \
    /org/freedesktop/UPower org.freedesktop.UPower.Hibernate"
alias rand="echo \$RANDOM"
alias ut="ssh 6365454829@un001.ecc.u-tokyo.ac.jp"
alias xunp="file-roller -h"
alias pc="sudo \paco -D"
alias pcalc="python -i -c 'from math import *' "
alias py3=python3
alias py2=python2
alias _reloadrc="test -f ~/.bashrc && source ~/.bashrc"
# alias mytime="date +%Y%m%d-%H%M%S"
alias sh="ENV=$HOME/.shrc PS1=\$\  sh"
# type trash >/dev/null 2>&1 && alias rm=trash

alias wic=wicd-curses
alias wil="wicd-cli -y -l | head"
alias wicn="wicd-cli -y -c -n"

alias aptin="apt-get install"
alias aptsearch="apt-cache search"
alias aptshow="apt-cache show"

alias yt=yaourt
null type pacman-color && {
    alias pacman=pacman-color
    export pacman_program=pacman-color # used by pacmatic
    export PACMAN=pacman-color         # used by yaourt
}
null type pacmatic && {
    alias pacman="pacmatic"
    export PACMAN="pacmatic"
}

alias ubuntu-upgrade="sudo apt-get autoremove --yes && sudo apt-get update --yes && sudo apt-get upgrade --yes"
alias arch-upgrade="sudo pacman -Syu"
alias port-upgrade="port selfupdate && port sync && port upgrade installed"

if iscygwin; then
    null type windate || alias windate="/c/Windows/System32/cmd.exe //c 'echo %DATE%-%TIME%'"
    alias cygsu="cygstart /cygwinsetup.exe"
    alias emacs="CYGWIN=tty emacs -nw"
    alias ls="ls -CFG $(iswindows || test "$TERM" == dumb || echo --color=auto)"
fi

alias g=git
if null type _git    # enable programmable completion for g
then
    complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
	|| complete -o default -o nospace -F _git g
fi

encrypt-stream(){
    test $# -eq 1 &&
    mcrypt --key $1 2>/dev/null | base64
}

decrypt-stream(){
    test $# -eq 1 &&
    base64 -d | mcrypt -d --key $1 2>/dev/null
}

showinfo(){
    echo "Japanese letters are 表示可能"

    __try_exec diskinfo

    ! isdarwin && test -n "${DISPLAY}" && {
        __try_exec xrandr | \grep --color=never ^Screen
    }

    iswindows || __try_exec finger $USER
    LANG=C __try_exec id
    __try_exec xset q
}

x(){
    if [[ -z $DISPLAY ]] && ! [[ -e /tmp/.X11-unix/X0 ]] && (( EUID )); then
        #mkdir -p ~/.backup/log
        # nohup startx >~/.backup/log/xorg.log 2>&1 &
        # exit
        exec startx
    else
        echo "X cant be started! Maybe another X is already running or something." 1>&2
    fi
}

bak(){
    for file in "$@"
    do
        cp -v ${file} ${file}.bak
    done
}

di(){
    if type colordiff >/dev/null 2>&1 && test $TERM != dumb
    then
        local diffcmd=colordiff
    else
        local diffcmd=diff
    fi
    ${diffcmd} -u "$@" | ${PAGER}
}

throw-away(){
    mkdir -p ~/.backup/tb
    for file in "$@"
    do
        mv $file ~/.backup/tb
    done
}

mkcd(){
    mkdir -p $1
    cd $1
}

catclip(){
    if iswindows
    then
        cat /dev/clipboard | tr -d \\r
    else
        xclip -o -selection "clipboard"
    fi
}

setclip(){
    if iswindows
    then
        if test $# -eq 0
        then
            sed -e 's/$/\r/' | tee /dev/clipboard
        else
            cat "$@" | sed -e 's/$/\r/' | tee /dev/clipboard
        fi
    else
        if test $# -eq 0
        then
            xclip -i -f -selection "primary" | xclip -i -f -selection "clipboard"
        else
            cat "$@" | xclip -i -f -selection "primary" | xclip -i -f -selection "clipboard"
        fi
    fi
}

if iswindows; then
    alias _open_file='cmd.exe //c start ""'
elif isdarwin; then
    alias _open_file=open
elif islinux; then
    if null type pcmanfm; then
        alias _open_file="LC_MESSAGES= pcmanfm"
    else
        alias _open_file="LC_MESSAGES= xdg-open"
    fi
else
    alias _open_file=cat
fi

o(){
    if test $# -eq 0
    then
        _open_file . >/dev/null 2>&1 &
    else
        for f in "$@"
        do
            if test -d $f
            then
                _open_file $f >/dev/null 2>&1 &
            else
                _open_file $f >/dev/null 2>&1 &
            fi
        done
    fi
}

convmv-sjis2utf8-test(){
    convmv -r -f sjis -t utf8 *
}

convmv-sjis2utf8-notest(){
    convmv -r -f sjis -t utf8 * --notest
}

_my_git_config(){
    git config --global user.name '10sr'
    git config --global user.email '8slashes+git@gmail.com'
    git config --global core.autocrlf false
    git config --global core.excludesfile '~/.gitignore'
    git config --global color.ui auto
    git config --global status.relativePaths false
    git config --global status.showUntrackedFiles normal
    git config --global alias.graph "log --graph --date-order -C -M --pretty=tformat:\"<%h> %ad [%an] %Cgreen%d%Creset %s\" --all --date=iso"
    git config --global alias.st "status -s -b"
    git config --global alias.b "branch"
    git config --global alias.ci "commit --verbose"
    git config --global alias.co "checkout"
    git config --global alias.cim "commit --verbose -m"
    git config --global alias.di "diff --color"
    git config --global alias.me "merge --no-ff --stat -v"
    git config --global alias.ls "ls-files -v --full-name"
    git config --global alias.sl "!sl"
    # git config --global alias.my-ls "ls-files | xargs ls"
    # git config --global alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
    git config --global alias.addi "add -i"
    if iswindows; then
        git config --global core.fileMode false
    fi
}

__my_parse_svn_branch() {
    local LANG=C
    local svn_url=$(svn info 2>/dev/null | sed -ne 's#^URL: ##p')
    local svn_repository_root=$(svn info 2>/dev/null | sed -ne 's#^Repository Root: ##p')
    echo ${svn_url} | sed -e 's#^'"${svn_repository_root}"'##g' | awk '{print $1}'
}

__my_svn_ps1(){
    if svn status >/dev/null 2>&1
    then
        local svn_branch=$(__my_parse_svn_branch)
        test -n "${svn_branch}" && printf "$1" "{$svn_branch}"
    fi
}

#Change ANSI Colors
_chengecolors(){
    echo -e \
        "\e]P0000000" \
        "\e]P1cd0000" \
        "\e]P200cd00" \
        "\e]P3cdcd00" \
        "\e]P41e90ff" \
        "\e]P5cd00cd" \
        "\e]P600cdcd" \
        "\e]P7353535" \
        "\e]P8666666" \
        "\e]P9ff9999" \
        "\e]Pa99ff99" \
        "\e]Pbffff99" \
        "\e]Pc9999ff" \
        "\e]Pdff99ff" \
        "\e]Pe99ffff" \
        "\e]Pfffffff"
}

# printf "\e]P7353535" \

_colors(){
    echo -e \
        "\e[30mBlack" \
        "\e[31mRed" \
        "\e[32mGreen" \
        "\e[33mYellow" \
        "\e[34mBlue" \
        "\e[35mMagenta" \
        "\e[36mCyan" \
        "\e[37mWhite"
    echo -e \
        "\e[30;1mBright Black" \
        "\e[31;1mBright Red" \
        "\e[32;1mBright Green" \
        "\e[33;1mBright Yellow" \
        "\e[34;1mBright Blue" \
        "\e[35;1mBright Magenta" \
        "\e[36;1mBright Cyan" \
        "\e[37;1mBright White\n" \
        "\e[0m"
}

_my_install_script(){
    mkdir -p $HOMO/bin/
    for f in "$@"
    do
        bn=$(basename "$f")
        type $bn >/dev/null 2>&1 || {
            wget "$f" -P "$HOME/bin/" &&
            chmod u+x "$HOME/bin/${bn}"
        }
    done
}
_my_install_script http://www.frexx.de/xterm-256-notes/data/colortable16.sh

winln(){
    # for windose make link (actually junction)
    if test $# -eq 0
    then
        {
            echo "usage: winln TARGET LINK_NAME"
            echo "Create a link to TARGET with the name LINK_NAME (that is, TARGET must already exist)."
            echo "About other features run 'junction'."
        } 1>&2
        return 1
    else
        junction "$2" "$1"
    fi
}

__my_battery_status(){
    local dir=/sys/class/power_supply/BAT0
    if test -d $dir
    then
        local st=$(cat $dir/status)
        local full=$(cat $dir/charge_full)
        local now=$(cat $dir/charge_now)
        local rate=$(expr $now \* 100 / $full)
        printf "$1" "${st}:${rate}%"
    fi
}
alias bat='__my_battery_status %s\\n'

ip-address(){
    local ip=$(LANG=C ifconfig | \grep --color=never "inet " | \grep --color=never -v "127.0.0.1" | awk '{print $2}')
    test -n "$ip" && printf $1 $ip
}

__my_prompt_function(){              # used by PS1
    # remove __try_exec from function
    local lastreturn=$?
    if test "${TERM}" == dumb
    then
        local c1=
        local c2=
        local c3=
        local cdef=
    else
        local c1="\e[33m"
        local c2="\e[36m"
        local c3="\e[37m"
        local cdef="\e[0m"
    fi
    if iswindows
    then
        local pwd=$PWD
        local oldpwd=$OLDPWD
        git branch >/dev/null 2>&1 && local git="[GIT]"
        local date=$(/c/Windows/System32/cmd.exe //c 'echo %DATE%-%TIME%')
    else
        local pwd=$(echo "${PWD}/" | sed -e "s#${HOME}#~#")
        local oldpwd=$(echo "${OLDPWD}/" | sed -e "s#${HOME}#~#")
        local jobnum=$(jobs | wc -l) # a strange job exists...
        local git=$(__try_exec __git_ps1 [GIT:%s])
        local date=$(LANG=C __try_exec date +"%a, %d %b %Y %T %z")
    fi
    local dirs=$(dirs | wc -l)
    # local svn=$(type svn >/dev/null 2>&1 && __try_exec __my_svn_ps1 [SVN:%s])
    if test -z "$DISPLAY" && ! iswindows
    then
        local ip=$(ip-address [Addr:%s])
        local bst="/tmp/${USER}-tmp/batterystatus"
        test -f $bst && local battery="[Battery:$(sed -e 's`%`%%`g' $bst)]"
        __my_battery_status %s >$bst &
    fi
    # local battery=$(battery-state [%s] | sed -e 's`%`%%`g') # very slow

    printf " [${c1}${pwd}${cdef}<${c3}${oldpwd}${cdef}]${git}${svn}${battery}${ip}\n"
    printf "${c2}${USER}@${HOSTNAME}${cdef} ${date}\n"
    printf "shlv:${SHLVL} dirs:${dirs} last:${lastreturn} "

}
__my_set_title(){
    title="$(echo $@)"
    case $TERM in
        (rxvt*|xterm*|aterm|screen*)
        test -t 1 &&
        test -n "$DISPLAY" &&
        test -z "$EMACS" &&
        echo -n -e "\033]0;${title}\007"
		;;
	esac
}
export PROMPT_COMMAND="__my_set_title \${USER}@\${HOSTNAME}\ \${PWD};"

# copied from https://wiki.archlinux.org/index.php/X_resources
invader(){
    # ANSI color scheme script featuring Space Invaders
    #
    # Original: http://crunchbanglinux.org/forums/post/126921/#p126921
    # Modified by lolilolicon
    #

    f=3 b=4
    for j in f b; do
        for i in {0..7}; do
            printf -v $j$i %b "\e[${!j}${i}m"
        done
    done
    bld=$'\e[1m'
    rst=$'\e[0m'

    cat << EOF

 $f1  ▀▄   ▄▀     $f2 ▄▄▄████▄▄▄    $f3  ▄██▄     $f4  ▀▄   ▄▀     $f5 ▄▄▄████▄▄▄    $f6  ▄██▄  $rst
 $f1 ▄█▀███▀█▄    $f2███▀▀██▀▀███   $f3▄█▀██▀█▄   $f4 ▄█▀███▀█▄    $f5███▀▀██▀▀███   $f6▄█▀██▀█▄$rst
 $f1█▀███████▀█   $f2▀▀███▀▀███▀▀   $f3▀█▀██▀█▀   $f4█▀███████▀█   $f5▀▀███▀▀███▀▀   $f6▀█▀██▀█▀$rst
 $f1▀ ▀▄▄ ▄▄▀ ▀   $f2 ▀█▄ ▀▀ ▄█▀    $f3▀▄    ▄▀   $f4▀ ▀▄▄ ▄▄▀ ▀   $f5 ▀█▄ ▀▀ ▄█▀    $f6▀▄    ▄▀$rst

 $bld$f1▄ ▀▄   ▄▀ ▄   $f2 ▄▄▄████▄▄▄    $f3  ▄██▄     $f4▄ ▀▄   ▄▀ ▄   $f5 ▄▄▄████▄▄▄    $f6  ▄██▄  $rst
 $bld$f1█▄█▀███▀█▄█   $f2███▀▀██▀▀███   $f3▄█▀██▀█▄   $f4█▄█▀███▀█▄█   $f5███▀▀██▀▀███   $f6▄█▀██▀█▄$rst
 $bld$f1▀█████████▀   $f2▀▀▀██▀▀██▀▀▀   $f3▀▀█▀▀█▀▀   $f4▀█████████▀   $f5▀▀▀██▀▀██▀▀▀   $f6▀▀█▀▀█▀▀$rst
 $bld$f1 ▄▀     ▀▄    $f2▄▄▀▀ ▀▀ ▀▀▄▄   $f3▄▀▄▀▀▄▀▄   $f4 ▄▀     ▀▄    $f5▄▄▀▀ ▀▀ ▀▀▄▄   $f6▄▀▄▀▀▄▀▄$rst


                                     $f7▌$rst

                                   $f7▌$rst

                              $f7    ▄█▄    $rst
                              $f7▄█████████▄$rst
                              $f7▀▀▀▀▀▀▀▀▀▀▀$rst

EOF
}
#/etc/lsb-release
