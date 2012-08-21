#!/bin/bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

##########################################
null(){
    "$@" >/dev/null 2>&1
}
__try_exec(){
    type $1 >/dev/null 2>&1 && "$@"
}

##########################
# system type

alias ismsys=false
alias iscygwin=false
alias iswindows=false
alias isdarwin=false
alias islinux=false
alias with_coreutils=false      # for mac

case `uname` in
    (MINGW*) alias ismsys=true ;;
    (CYGWIN*) alias iscygwin=true ;;
    (Darwin*) alias isdarwin=true ;;
    (Linux*) alias islinux=true ;;
esac
null ls --version && alias with_coreutils=true

( ismsys || iscygwin ) && alias iswindows=true

export PS1                      # PS1 is defined later
# PROMPT_COMMAND=prompt_function
if false iswindows
then
    export PAGER='tr -d \\r | less'
else
    export PAGER="less"
fi
export LESS="-iRMXF"

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
echo "$TERM" | grep '^screen' >/dev/null 2>&1 || export TERM_ORIG=$TERM

test -z "$TMP" && export TMP=/tmp/${USER}-tmp
mkdir -p "$TMP"

! iswindows && null type stty && {
    stty stop undef        # unbind C-s to stop displaying output
    # stty erase '^h'
}

if iswindows; then
    # export PS1=" \[\e[32m\]\u@\H \[\e[33m\]\w\[\e[0m\] \d \t\n\s \# \j \$ "
    # export PS1=" [\[\e[33m\]\w\[\e[0m\]]\n\[\e[32m\]\u@\H\[\e[0m\] \d \t \s.\v\nhist:\# jobs:\j \$ "
    export USER=$USERNAME
fi


#######################

iswindows && alias tty="echo cmd.exe"
type fortune >/dev/null 2>&1 && {
    echo
    fortune
    echo
    fortune -o
    echo
}
uname -a
echo TERM $TERM $(tput colors) colors connected to $(tty), running $BASH $BASH_VERSION
echo

###################################
# some aliases and functions

( ! with_coreutils && isdarwin ) || test "$TERM" == dumb || _coloroption=" --color=always"
( ! with_coreutils && isdarwin ) || iswindows || _timeoption=" --time-style=long-iso"

alias ls="ls -hCF${_coloroption}${_timeoption}"
# export GREP_OPTIONS=""
alias grep="grep -n${_coloroption}"
iswindows && alias grep="grep -n"
# alias ll="ls -l"
# alias la="ls -A"
# alias lla="ls -Al"
# alias less=""
alias em="emacs -nw"
null type vim && alias vi=vim
alias pstree="LANG=C pstree"
alias cp="cp -v"
alias mv="mv -v"
alias psaux="ps auxww"
alias q=exit
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
alias xunp="file-roller -h"
alias pc="sudo \paco -D"
alias pycalc="python -i -c 'from math import *' "
alias py3=python3
alias py2=python2
alias _reloadrc="test -f ~/.bashrc && source ~/.bashrc"
# alias mytime="date +%Y%m%d-%H%M%S"
alias sh="ENV=$HOME/.shrc PS1=\$\  PROMPT_COMMAND="" sh"
# type trash >/dev/null 2>&1 && alias rm=trash
alias mpg123="mpg123 -C -v --title"
export PLAYER="mpg123 -C -v --title"
alias screen="screen -e^z^z"
alias zcd="cd \`zenity --file-selection --directory\`"

alias pad=notepad
null type gedit && alias pad=gedit
null type leafpad && alias pad=leafpad

alias wic=wicd-curses
alias wil="wicd-cli -y -l | head"
#alias wicn="wicd-cli -y -c -n"
wicn(){
    if test $# -eq 0
    then
        local num
        wicd-cli -y -l | head
        echo -n "input num: "
        read num
        test -n "$num" && wicd-cli -y -c -n $num
    else
        wicd-cli -y -c -n $1
    fi
}

alias aptin="apt-get install"
alias aptsearch="apt-cache search"
alias aptshow="apt-cache show"

for f in /usr/share/vim/vimcurrent/macros/less.sh \
    /usr/share/vim/vim73/macros/less.sh
do
    test -f $f && alias vl=$f && break
done

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

input(){
    local foo
    stty -echo
    read foo
    stty echo
    echo $foo
}

tmux(){
    if test $# -eq 0
    then
        if command tmux has -t main
        then
            command tmux attach -t main
        else
            command tmux new -s main
        fi
    else
        command tmux "$@"
    fi
}

__my_moc_state(){
    type mocp >/dev/null 2>&1 || return
    test "`mocp -Q %state 2>/dev/null`" == PLAY || return
    printf "$1" "`mocp -Q %title 2>/dev/null`"
}

mcrypt-stream(){
    test $# -eq 2 || return 1
    case $1 in
        en)
            mcrypt --key $2 | base64 ;;
        de)
            base64 -d | mcrypt -d --key $2 ;;
    esac
}

gpg-stream(){
    test $# -eq 2 || return 1
    case $1 in
        en)
            gpg --passphrase $2 -c --batch |base64 ;;
        de)
            base64 -d|gpg --passphrase $2 -d --batch ;;
    esac
}

alias enst="gpg-stream en"
alias dest="gpg-stream de"

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
        #mkdir -p ~/.my/log
        # nohup startx >~/.my/log/xorg.log 2>&1 &
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

tb(){
    local tb=~/.my/tb
    mkdir -p $tb
    for file in "$@"
    do
        mv $file $tb
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
    elif isdarwin
    then
        pbpaste
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
    elif isdarwin
    then
        if test $# -eq 0
        then
            pbcopy
        else
            cat "$@" | pbcopy
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

_open_file(){
    if iswindows
    then
        cmd.exe //c start "" "$@"
    elif isdarwin
    then
        open "$@"
    elif islinux
    then
        if null type pcmanfm; then
            LC_MESSAGES= pcmanfm "$@"
        else
            LC_MESSAGES= xdg-open "$@" &
        fi
    else
        cat "$@"
    fi
}

o(){
    if test $# -eq 0
    then
        _open_file .
    else
        for f in "$@"
        do
            _open_file "$f"
        done
    fi
}

convmv-sjis2utf8-test(){
    convmv -r -f sjis -t utf8 *
}

convmv-sjis2utf8-notest(){
    convmv -r -f sjis -t utf8 * --notest
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
    test type ifconfig >/dev/null 2>&1 || return 1
    local ip=$(LANG=C ifconfig | \grep --color=never "inet " | \grep --color=never -v "127.0.0.1" | awk '{print $2}')
    test -n "$ip" && printf $1 $ip
}

TMUX_CURRENT="$(__try_exec tmux display -p '#S:#I:#W.#P')"
__my_ps1_tmux(){
    local last=$?
    test -n "$TMUX" && echo "[TMUX:$TMUX_CURRENT]"
    return $last
}
__my_ps1_moc(){
    local last=$?
    __my_moc_state "[MOC:%s]"
    return $last
}
__my_ps1_git(){
    local last=$?
    null type __git_ps1 || return $last
    __try_exec __git_ps1 "[GIT:$(__try_exec git config --get user.name):%s]"
    return $last
}
__my_ps1_ipaddr(){
    local last=$?
    test -z "$DISPLAY" && ! iswindows && ip-address [Addr:%s]
    return $last
}
__my_ps1_bttry(){
    local last=$?
    local bst="${TMP}/batterystatus"
    if test -z "$DISPLAY" && ! iswindows
    then
        test -f $bst && local bstr="$(cat $bst)"
        test -n "$bstr" && echo "[Battery:$bstr]"
        __my_battery_status %s >$bst &
    fi
    return $last
}
__my_ps1_dirs(){
    dirs | wc -l
}
__my_ps1_jobs(){
    jobs | wc -l
}
if test "$TERM" != dumb
then
    __my_c1="\[\e[1;31m\]"       # color for PWD
    __my_c2="\[\e[0;36m\]"         # color for user
    __my_c3="\[\e[1;30m\]"       # color for OLDPWD
    __my_c4="\[\e[1;32m\]"       # color for ::
    __my_cdef="\[\e[0m\]"
fi
_PS1="\
${__my_c4}:: ${__my_cdef}[${__my_c1}\w/${__my_cdef}<${__my_c3}\${OLDPWD}${__my_cdef}]\$(__my_ps1_tmux)\$(__my_ps1_git)\$(__my_ps1_bttry)\$(__my_ps1_ipaddr)\$(__my_ps1_moc)\n\
${__my_c4}:: ${__my_c2}\u@\H${__my_cdef} \D{%a, %d %b %Y %T %z} ${SHELL} \V\n\
${__my_c4}:: ${__my_cdef}shlv:${SHLVL} cnum:\# jobs:\j last:\$? \$ "
PS1=$_PS1

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
