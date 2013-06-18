#!/bin/bash

# TODO: use tput

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

alias inbash=false
alias inzsh=false

if test -n "$BASH_VERSION"
then
    alias inbash=true
elif test -n "$ZSH_VERSION"
then
    alias inzsh=true
fi

#################################

if echo $PATH | grep "$HOME" >/dev/null 2>&1
then
    PATH="${PATH}:${HOME}/.local/bin"
fi

if false iswindows
then
    export PAGER='tr -d \\r | less'
else
    export PAGER="less"
fi
export LESS="-iRMX"

_src_hilite_lp_path="`which src-hilite-lesspipe.sh 2>/dev/null`"
if test -n "$_src_hilite_lp_path"
then
    export LESSOPEN="| $_src_hilite_lp_path %s"
fi

if null type vim
then
    export EDITOR=vim
else
    export EDITOR=vi
fi
export LANG=ja_JP.UTF-8
export LC_MESSAGES=C
# export CDPATH=".:~"
export VISUAL="$EDITOR"
export GIT_PAGER="less -F"
export GIT_EDITOR="$EDITOR"
if test -n "$TMUX" && \
    echo $TERM | grep screen >/dev/null 2>&1 && \
    tmux display -p '#{client_termname}' | grep 256color >/dev/null 2>&1
then
    TERM=screen-256color
fi

test -z "$TMP" && export TMP=/tmp/${USER}-tmp
mkdir -p "$TMP"

! iswindows && null type stty && {
    stty stop undef        # unbind C-s to stop displaying output
    # stty erase '^h'
}

if iswindows; then
    export USER=$USERNAME
fi

_tmux_prefs(){
    null type tmux || return 1
    tmux set -g mode-keys vi
}

if test -d ~/dbx
then
    export CHIT_PATH="$HOME/dbx/.chit"
fi

#######################

iswindows && alias tty="echo cmd.exe"
type fortune >/dev/null 2>&1 && {
    fortune
    echo
    fortune -o
    echo
}
uname -a
echo TERM $TERM $(tput colors) colors connected to $(tty), \
    running $BASH $BASH_VERSION
if test -n "$TMUX"
then
    tmux display -p 'Using tmux #S:#I:#W.#P, client is #{client_termname}' \
        2>/dev/null
    echo
fi

###################################
# some aliases and functions

( ! with_coreutils && isdarwin ) || test "$TERM" = dumb || \
    _coloroption=" --color=auto"
( ! with_coreutils && isdarwin ) || iswindows || \
    _timeoption=" --time-style=long-iso"
( ! with_coreutils && isdarwin ) || _hideoption=" --hide=[A-Z]*" # do not use

_timeformat_iso="%Y-%m-%dT%H:%M:%S%z"
_timeformat_rfc2822="%a, %d %b %Y %T %z"
_timeformat_num="%Y%m%d%H%M%S"
alias datenum="date +$_timeformat_num"

alias ls="ls -hCF${_coloroption}${_timeoption}"
if ! with_coreutils
then
    export LSCOLORS=gxfxcxdxbxegedabagacad
    alias ls="ls -G"
fi
# export GREP_OPTIONS=""
alias gr="grep -n --color=always"
iswindows && alias grep="grep -n"
# alias ll="ls -l"
# alias la="ls -A"
# alias lla="ls -Al"
alias less="less -F"
null type emacs && alias em="emacs -nw"
null type vim && alias vi=vim
alias pstree="LANG=C pstree"
alias cp="cp -v"
alias mv="mv -v"
alias rm="rm -v"
alias psaux="ps auxww"
alias q=exit
null type e3em && alias e3=e3em
#alias dirs="dirs -v -l | \grep -v \$(printf '%s$' \$PWD)"
alias po=popd
alias pu=pushd
null type sudo && alias sudo="sudo "              # use aliases through sudo
null type sudoedit && alias sudoe="sudoedit"
null type halt && alias halt="sudo halt"
null type reboot && alias reboot="sudo reboot"
null type dbus-send && {
    alias suspend="dbus-send --system --print-reply --dest=org.freedesktop.UPower \
    /org/freedesktop/UPower org.freedesktop.UPower.Suspend"
    alias hibernate="dbus-send --system --print-reply --dest=org.freedesktop.UPower \
    /org/freedesktop/UPower org.freedesktop.UPower.Hibernate"
}
alias rand="echo \$RANDOM"
null type file-roller && alias xunp="file-roller -h"
null type paco && alias pc="sudo \paco -D"
alias pycalc="python -i -c 'from math import *' "
null type python3 && alias py3=python3
null type python2 && alias py2=python2
alias _reloadrc="test -f ~/.bashrc && source ~/.bashrc"
# alias mytime="date +%Y%m%d-%H%M%S"
alias sh="ENV=$HOME/.shrc PS1=\$\  PROMPT_COMMAND="" sh"
# type trash >/dev/null 2>&1 && alias rm=trash
null type mpg123 && alias mpg123="mpg123 -C -v --title"
null type xmms2 && alias xm="xmms2"
#export PLAYER="mpg123 -C -v --title"

null type screen && alias screen="screen -e^z^z"
#alias zcd="cd \`zenity --file-selection --directory\`"
null type gtags && alias gtags="gtags --verbose"
null type htags && alias htags="htags --xhtml --symbol --line-number \
--frame --alphabet --verbose"
null type aunpack && alias au=aunpack
null type lv && alias lv="lv|less"

isdarwin && alias updatedb="LC_ALL=C updatedb"
# do not use locate installed by macports
isdarwin && test -x /usr/bin/locate && alias locate="/usr/bin/locate"

# pad
alias pad=notepad
null type gedit && alias pad=gedit
null type leafpad && alias pad=leafpad
isdarwin && alias pad="open -e"

null type wicd-curses && alias wic=wicd-curses
null type wicd-cli && alias wil="wicd-cli -y -l | head"
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

for f in /usr/share/vim/vimcurrent/macros/less.sh \
    /usr/share/vim/vim73/macros/less.sh \
    /usr/share/vim/vim72/macros/less.sh
do
    test -f $f && alias vl=$f && break
done

null type yaourt && alias yt=yaourt
null type cower && alias cower="cower --color=auto"
null type pacmatic && {
    alias pacman="pacmatic"
    export PACMAN="pacmatic"
}

_pacman_update_mirrorlist_with_reflector(){
    ml=/etc/pacman.d/mirrorlist
    cmd="$(expr "$(grep reflector $ml)" : '# With: *\(.*\)')"
    if test -z "$cmd"
    then
        cmd="reflector --verbose -l 5 --sort rate --save /etc/pacman.d/mirrorlist"
    fi
    sudo $cmd
}
null type reflector && test -f /etc/pacman.d/mirrorlist && \
    alias reflect_mirrorlist=_pacman_update_mirrorlist_with_reflector

null type apt-get && {
    alias aupgrade="sudo apt-get autoremove --yes && \
sudo apt-get update --yes && sudo apt-get upgrade --yes"
    alias aptin="apt-get install"
    alias aptsearch="apt-cache search"
    alias aptshow="apt-cache show"
}

null type port && {
    alias port="port -v"
    alias pupgrade="sudo port -v selfupdate && \
{ sudo port -v upgrade outdated; }"
}

if iscygwin; then
    null type windate || \
        alias windate="/c/Windows/System32/cmd.exe //c 'echo %DATE%-%TIME%'"
    alias cygsu="cygstart /cygwinsetup.exe"
    alias emacs="CYGWIN=tty emacs -nw"
    alias ls="ls -CFG $(iswindows || test "$TERM" = dumb || echo --color=auto)"
fi

g(){
    if test $# -eq 0 && null type git-info
    then
        git info
    else
        git "$@"
    fi
}
if null type _git && inbash
then
    # enable programmable completion for g
    complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
        || complete -o default -o nospace -F _git g
fi
git svn --help >/dev/null 2>&1 && alias gsvn="git svn"
null type gitmemo && alias m=gitmemo

null type gitmemo && alias m=gitmemo

alias setup.py="sudo python3 setup.py install --record files.txt"

ssh(){
    __my_set_screen_title ssh
    command ssh "$@"
}

clk(){
    local tformat="%Y/%m/%d %H:%M:%S %z"
    cal
    REPLY=
    printf "\\r`date "+${tformat}"`"
    read -t 1
    while test $? -ne 0
    do
        printf "\\r`date "+${tformat}"`"
        read -t 1
    done
}

s(){
    if test $# -eq 0
    then
        echo "No search word given." 1>&2
        return 1
    fi

    if git rev-parse --git-dir >/dev/null 2>&1
    then
        git grep -n "$@"
    elif which ag >/dev/null
    then
        ag --pager="$PAGER" "$@"
    elif which ack >/dev/null
    then
        ack --pager="$PAGER" "$@"
    else
        grep -nH --exclude='.svn/*' --exclude='.git/*' "$@" -r . | $PAGER
        # echo "No search command found. Use grep." 2>&1
        # return 127
    fi
}

man(){
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;35m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

scr(){
    test -n "$1" && pf="${1}-"
    local _tformat="%Y%m%d-%H%M%S%z"
    local _file="${HOME}/${pf}`date +${_tformat}`.script"
    SCRIPT=${_file} script ${_file} "$@"
}

netwait(){
    while ! ping -c 1 -t 1 example.com
    do
        true
    done
    echo network works.
}

cd(){
    if test $# -eq 0
    then
        pushd ~/ >/dev/null
    elif test "$1" = -
    then
        local pwd="$PWD"
        command cd "$OLDPWD"
        pushd -n "$pwd" >/dev/null        # stack last dir
    elif ! test -d "$1"
    then
        echo `basename ${SHELL}`: cd: "$1": No such file or directory  1>&2
        return 1
    else
        pushd "$1" >/dev/null
    fi
    __dirs_rm_dup "$PWD"
    echo "$PWD"
}

__dirs_rm_dup(){
    for d in "$@"
    do
        local next="$(__realpath --strip "$d")"
        for l in $(\dirs -v -l | cut -d $'\n' -f 2- | \
            \grep -x " *[0-9]\+ \+${next}" | \grep -o "^ *[0-9]\+ " | tac)
        do
            popd +$l -n >/dev/null
        done
    done
}

__realpath(){
    if type realpath >/dev/null 2>&1
    then
        command realpath "$@"
    else
        while ! test -d $1
        do
            shift
        done
        local d="$OLDPWD"
        command cd "$1"
        echo "$PWD"
        command cd "$d"
    fi
}

dh(){
    if test $# -eq 0
    then
        dirs -v -l
    else
        local dir="$(dirs -v -l | \grep "^ *$1 \+" | sed "s/^ *[0-9]* *//g")"
        cd "$dir"
    fi
}
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
        (cd ~; command tmux start;)
        if command tmux has -t main
        then
            command tmux attach -t main
        else
            (cd ~; command tmux new -s main;)
        fi
    else
        command tmux "$@"
    fi
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
dgpg(){
    if test "$1" = help || test -z "$2"
    then
        echo "dgpg: dgpg <en|de> <src-suffix> [<dst-suffix>]" 1>&2
        return
    fi
    local srcs="$2"
    local dsts="$3"
    test -z "$dsts" && dsts="${srcs}.out"
    local pw
    echo -n "dgpg pw: "
    read -s pw
    echo ""
    test -z "$pw" && return 1
    for f in *${srcs}
    do
        local d="$(basename "$f" "${srcs}")${dsts}"
        echo -n "Processing $f to $d..."
        if test -d "$f"
        then
            echo "`printf 'failed (%s is directory)' $f`"
        elif test -f "$d"
        then
            echo "`printf 'failed (%s is already exists)' $d`"
        elif <"$f" gpg-stream $1 $pw >"$d" 2>/dev/null
        then
            echo "done"
        else
            echo "failed"
            test -f "$d" && rm "$d"
        fi
    done
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
        echo "X cant be started! Another X is already running?" 1>&2
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
    local tb="$HOME/.my/tb"
    mkdir -p "$tb"
    for file in "$@"
    do
        mv -t "$tb" "$file"
    done
}

mkcd(){
    if test -d "$1"
    then
        echo "Dir \"$1\" already exists."
    else
        mkdir -p "$1"
        echo "Dir \"$1\" created."
    fi
    cd "$1"
}

if test -n "$TMUX" && null type reattach-to-user-namespace
then
    alias pbpaste="reattach-to-user-namespace pbpaste"
    alias pbcopy="reattach-to-user-namespace pbcopy"
fi

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
    if test $# -eq 0
    then
        exec 3<&0
    else
        exec 3<<__EOF__
`cat "$@"`
__EOF__
    fi
    if iswindows
    then
        0<&3 sed -e 's/$/\r/' | tee /dev/clipboard
    elif isdarwin
    then
        pbcopy 0<&3
    else
        0<&3 xclip -i -f -selection "primary" | \
            xclip -i -f -selection "clipboard"
    fi
    exec 3<&-
}

open_file(){
    if iswindows
    then
        cmd.exe //c start "" "$@"
    elif isdarwin
    then
        touch "$@"
        open "$@"
    elif islinux
    then
        touch "$@"
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
        open_file .
    else
        for f in "$@"
        do
            open_file "$(realpath "$f")"
        done
    fi
}

convmv-sjis2utf8-test(){
    convmv -r -f sjis -t utf8 *
}

convmv-sjis2utf8-notest(){
    convmv -r -f sjis -t utf8 * --notest
}

winln(){
    # for windose make link (actually junction)
    if test $# -eq 0
    then
        {
            echo "usage: winln TARGET LINK_NAME"
            echo "Create a link to TARGET with the name LINK_NAME \
(that is, TARGET must already exist)."
            echo "About other features run 'junction'."
        } 1>&2
        return 1
    else
        junction "$2" "$1"
    fi
}

__my_moc_state(){
    type mocp >/dev/null 2>&1 || return
    test "`mocp -Q %state 2>/dev/null`" = PLAY || return
    printf "$1" "`mocp -Q %title 2>/dev/null`"
}

__my_parse_svn_branch() {
    local LANG=C
    local svn_url=$(svn info 2>/dev/null | sed -ne 's#^URL: ##p')
    local svn_repository_root=$(svn info 2>/dev/null | \
        sed -ne 's#^Repository Root: ##p')
    echo ${svn_url} | sed -e 's#^'"${svn_repository_root}"'##g' | \
        awk '{print $1}'
}

__my_svn_ps1(){
    if svn status >/dev/null 2>&1
    then
        local svn_branch=$(__my_parse_svn_branch)
        test -n "${svn_branch}" && printf "$1" "{$svn_branch}"
    fi
}

__my_battery_status(){
    local dir=/sys/class/power_supply/BAT0
    if test -d $dir && test -r $dir/status && test -r $dir/charge_full && \
        test -r $dir/charge_now
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
    type ip >/dev/null 2>&1 || return 1
    local ip=$(LANG=C ip addr show scope global | \
        \grep --color=never --only-matching 'inet [^ ]*' | cut -d " " -f 2)
    test -n "$ip" && printf $1 $ip
}

test -n "$SCRIPT" && __my_ps1_script_str="${__my_c5}SCR${__my_cdef} "

test -n "$SSH_CONNECTION" && __my_ps1_ssh_str="${__my_c5}SSH${__my_cdef} "

__my_ps1_scale(){
    local last=$?
    printf "${LINES}x${COLUMNS}"
    return $last
}

__my_ps1_tmux(){
    local last=$?
    null type tmux || return $last
    local tmuxc="$(tmux display -p '#S:#I:#W.#P' 2>/dev/null)"
    test -n "$TMUX" && echo "[TMUX:$tmuxc]"
    return $last
}

__my_ps1_moc(){
    local last=$?
    __my_moc_state "[MOC:%s]"
    return $last
}

for f in /usr/share/git/git-prompt.sh \
    /opt/local/share/git-core/git-prompt.sh \
    /opt/local/share/doc/git-core/contrib/completion/git-prompt.sh
do
    test -r $f && . $f && break
done
GIT_PS1_SHOWDIRTYSTATE=t
GIT_PS1_SHOWUPSTREAM=t
__my_ps1_git(){
    local last=$?
    null type __git_ps1 || return $last
    null __gitdir || return $last
    __git_ps1 "[GIT:$(__try_exec git config --get user.name):%s]"
    return $last
}

__my_ps1_ipaddr(){
    local last=$?
    ! iswindows && ip-address [Addr:%s]
    return $last
}

__my_ps1_bttry(){
    local last=$?
    local bst="${TMP}/batterystatus"
    if test -z "$DISPLAY" && ! iswindows
    then
        test -f $bst && local bstr="$(cat $bst)"
        test -n "$bstr" && ! echo $bstr | grep 100 >/dev/null 2>&1 && \
            echo "[Battery:$bstr]"
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
    __my_c1="\[\e[0;33m\]"       # color for PWD
    __my_c2="\[\e[0;36m\]"         # color for user
    __my_c3="\[\e[1;30m\]"       # color for OLDPWD
    if test "`hostname`" = arch-aspireone; then __my_c4="\[\e[1;34m\]"
    elif test "`hostname`" = darwin-mba.local; then __my_c4="\[\e[1;31m\]"
    elif test "`hostname`" = newkiwi; then __my_c4="\[\e[1;35m\]"
    else __my_c4="\[\e[1;32m\]"       # color for ::
    fi
    __my_c5="\[\e[30;47m\]"        # color for SCR
    __my_cdef="\[\e[0m\]"
fi

export _LAST_STATUS=$?
__my_export_last_status(){
    export _LAST_STATUS=$?
    echo $_LAST_STATUS
    return $_LAST_STATUS
}

_ps1_bash="\
${__my_c4}:: ${__my_cdef}[${__my_c2}\u@\H${__my_cdef}:${__my_c1}\w/${__my_cdef}]\$(__my_ps1_git)\$(__my_ps1_bttry)\$(__my_ps1_ipaddr)\$(__my_ps1_moc)\n\
${__my_c4}:: ${__my_cdef}l${SHLVL}n\#j\js\$? $(__my_ps1_scale) \D{%T} ${__my_ps1_script_str}${__my_ps1_ssh_str}\$ "
inbash && PS1=$_ps1_bash

__my_set_screen_title(){
    if test -n "$TMUX" && test -z "$INSIDE_EMACS"
    then
        echo -ne "\033k$1\033\\"
    fi
}

__my_set_title(){
    case $TERM in
        (rxvt*|xterm*|aterm|screen*)
            title="$(echo $@)"
            test -t 1 &&
            test -n "$DISPLAY" &&
            test -z "$EMACS" &&
            echo -n -e "\033]0;${title}\007"
            ;;
    esac
        }
        PROMPT_COMMAND="__my_set_title \${USER}@\${HOSTNAME}\:\${PWD};
__my_set_screen_title \$(basename \"\$PWD\")/"
