#!/bin/bash

##########################
# system type

alias ismsys=false
alias iscygwin=false
alias iswindows="iscygwin || ismsys"
alias isdarwin=false
alias islinux=false

case `uname` in
    (MINGW32*) alias ismsys=true ;;
    (CYGWIN*) alias iscygwin=true ;;
    (Darwin*) alias isdarwin=true ;;
    (Linux*) alias islinux=true ;;
esac

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

if false null type vim
then
    export EDITOR=vim
else
    export EDITOR=vi
fi
export LC_MESSAGES=C
export CDPATH=".:~"
export VISUAL="$EDITOR"
export GIT_PAGER="$PAGER"
export GIT_EDITOR="$EDITOR"

null type stty && {
    stty stop undef        # unbind C-s to stop displaying output
    stty erase '^h'
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

if [ "${EMACS}" = "t" ]; then   # for emacs shell
    true export PS1="\u@\H \d \t \w\nemacs shell\$ "
elif echo "$EMACS" | grep term >/dev/null 2>&1; then # for emacs term
    echo "Emacs Term"
fi

# if test -f /etc/issue
# then
#     cat /etc/issue
# fi

###################################
# some aliases and functions

test "$TERM" == dumb || _ENABLECOLOR="--color=always "

export LESS="-iRMX"
export GREP_OPTIONS="${_ENABLECOLOR}"
alias ls="ls -hCF ${_ENABLECOLOR}--time-style=long-iso"
# alias ll="ls -l"
# alias la="ls -A"
# alias lla="ls -Al"
# alias less=""
alias vl=/usr/share/vim/vimcurrent/macros/less.sh
alias em="emacs -nw"
alias pstree="LANG=C pstree"
# alias apt-get="sudo apt-get"
alias ut="ssh 6365454829@un001.ecc.u-tokyo.ac.jp"
alias rand="echo \$RANDOM"
alias xunp="file-roller -h"
alias pacome="sudo \paco -D"
alias psall="ps auxww"
alias q=exit
alias p="$PAGER"
alias c=cat
alias pcalc="python -i -c 'from math import *' "
alias py3=python3
alias _reloadrc="test -f ~/.bashrc && source ~/.bashrc"
alias sudo="sudo "              # use aliases through sudo
alias e3=e3em
alias mytime="date +%Y%m%d-%H%M%S"
alias sh="ENV=$HOME/.shrc PS1=\$\  sh"
alias halt="sudo halt"
alias reboot="sudo reboot"
# type trash >/dev/null 2>&1 && alias rm=trash

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
    alias pacman=pacmatic
    export PACMAN=pacmatic
}

alias ubuntu-upgrade="sudo apt-get autoremove --yes && sudo apt-get update --yes && sudo apt-get upgrade --yes"
alias arch-upgrade="yaourt -Syu"
alias port-upgrade="port selfupdate && port sync && port upgrade installed"

if iscygwin; then
    null type windate || alias windate="/c/Windows/System32/cmd.exe //c 'echo %DATE%-%TIME%'"
    alias cygsu="cygstart /cygwinsetup.exe"
    alias emacs="CYGWIN=tty emacs -nw"
    alias ls="ls -CFG $(test "$TERM" == dumb || echo --color=auto)"
fi

alias g=git
if null type _git    # enable programmable completion for g
then
    complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
	|| complete -o default -o nospace -F _git g
fi

showinfo(){
    echo "Japanese letters are 表示可能"

    __try_exec diskinfo

    ! isdarwin && test -n "${DISPLAY}" && {
        __try_exec xrandr | grep --color=never ^Screen
    }

    iswindows || __try_exec finger $USER
    LANG=C __try_exec id
    __try_exec xset q
}

x(){
    if [[ -z $DISPLAY ]] && ! [[ -e /tmp/.X11-unix/X0 ]] && (( EUID )); then
        nohup startx >~/.backup/log/xorg.log 2>&1 &
    else
        echo "X cant be started! Maybe another X is already running!" 1>&2
    fi
}

export __MYGITBAREREP="${HOME}/dbx/.git-bare"
git-make-local-rep(){
    test $# -eq 0 && {
        echo "specify repository name." 1>&2
        return 1
    }

    dir="${__MYGITBAREREP}/$1.git"
    cdir=$PWD

    if test -d "$dir"
    then
        echo "dir $dir already exist!" 1>&2
    else
        mkdir -p "$dir" && {
            cd "$dir" &&
            git init --bare --shared=all
        }
    fi

    cd ${cdir}
}

bak(){
    for file in "$@"
    do
        mv -v ${file} ${file}.bak
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
o(){
    if [ $# -eq 0 ]
    then
        local f=.
    else
        local f="$1"
    fi
    if iswindows
    then
        cmd.exe //c start "" "$f"
    elif isdarwin
    then
        open "$f"
    elif type pcmanfm >/dev/null 2>&1
    then
        pcmanfm "$f"
    else
        xdg-open "$f"
    fi
}
convmv-sjis2utf8-test(){
    convmv -r -f sjis -t utf8 *
}
convmv-sjis2utf8-notest(){
    convmv -r -f sjis -t utf8 * --notest
}
_mygitconfig(){
    git config --global user.name '10sr'
    git config --global user.email '8slashes+git@gmail.com'
    git config --global core.autocrlf false
    git config --global color.ui auto
    git config --global status.relativePaths false
    git config --global status.showUntrackedFiles normal
    git config --global alias.graph "log --graph --date-order -C -M --pretty=format:\"<%h> %ad [%an] %Cgreen%d%Creset %s\" --all --date=short"
    git config --global alias.st "status -s"
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
# http://www.frexx.de/xterm-256-notes/data/colortable16.sh

_install_script(){
    mkdir -p $HOMO/bin/
    for f in "$@"
    do
        bn=$(basename "$f")
        type ${bn} >/dev/null 2>&1 || wget "$f" -P "$HOME/bin/"
        chmod u+x "$HOME/bin/${bn}"
    done
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

battery-status(){
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
alias bat='battery-status %s\\n'

battery-status2(){
    local dir=/sys/class/power_supply/BAT0
    . $dir/uevent
    local rate=$(expr $POWER_SUPPLY_CHARGE_NOW \* 100 / $POWER_SUPPLY_CHARGE_FULL)
    echo ${POWER_SUPPLY_STATUS}:${rate}%
}

ip-address(){
    local ip=$(LANG=C ifconfig | grep "inet " | grep -v "127.0.0.1" | awk '{print $2}')
    test -n "$ip" && printf $1 $ip
}

__my_prompt_function(){              # used by PS1
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
        local jobnum=$(jobs | wc -l)
        local git=$(__try_exec __git_ps1 [GIT:%s])
        local date=$(LANG=C __try_exec date +"%a, %d %b %Y %T %z")
    fi
    # local svn=$(type svn >/dev/null 2>&1 && __try_exec __my_svn_ps1 [SVN:%s])
    if test -z "$DISPLAY"
    then
        local ip=$(ip-address [Addr:%s])
        test -f /tmp/batterystatus && local battery="[Battery:$(sed -e 's`%`%%`g' /tmp/batterystatus)]"
        battery-status %s >/tmp/batterystatus &
    fi
    local tty=$(__try_exec tty | sed -e 's:/dev/::')
    # local battery=$(battery-state [%s] | sed -e 's`%`%%`g') # very slow
    printf " [${c1}${pwd}${cdef}<${c3}${oldpwd}${cdef}]${git}${svn}${battery}${ip}\n"
    printf "${c2}${USER}@${HOSTNAME}${cdef} ${tty} ${date} ${BASH} ${BASH_VERSION}\n"
    printf "shlv:${SHLVL} jobs:${jobnum} last:${lastreturn} "

}

# from https://wiki.archlinux.org/index.php/X_resources
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
