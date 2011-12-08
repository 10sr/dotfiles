#!/bin/bash
# 外部ファイルの読み込み
# test -r ~/filepath && . ~/filepath
# ln SRC DST
##########################################
safe-cmd(){
    type $1 >/dev/null 2>&1 && "$@"
}

test -r /etc/bashrc && . /etc/bashrc

# export PS1="\[\e[32m\]\u@\H \[\e[33m\]\w\[\e[0m\] \d \t\n\s \# \j \$ "
# export PS1="[\[\e[33m\]\w\[\e[0m\]]\n\[\e[32m\]\u@\H\[\e[0m\] \d \t \s.\v\nhist:\# jobs:\j \$ "
export PS1="\$(prompt_function)\$ "
# PROMPT_COMMAND=prompt_function
export PAGER="less"
export EDITOR="vim"
export VISUAL=$EDITOR
export LESS="-iRM"
# export LC_MESSAGES="C"
# export LANG=ja_JP.UTF-8
# export CDPATH=".:~"             # 使い方がよく分からない
export GIT_PAGER=$PAGER
export GIT_EDITOR=$EDITOR

alias ls="ls -CFG $(test "$TERM" == dumb || echo --color=auto) --time-style=long-iso"
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -Al"
alias vl=/usr/share/vim/vimcurrent/macros/less.sh
alias em="emacs -nw"
alias apt-get="sudo apt-get"
alias aptin="apt-get install"
alias aptsearch="apt-cache search"
alias aptshow="apt-cache show"
alias ut="slogin t110414@un001.ecc.u-tokyo.ac.jp"
alias rand="echo \$RANDOM"
alias xunp="file-roller -h"
alias pacome="sudo \paco -D"
alias destroy="rm -rf"
alias psall="ps auxww"
alias g=git
alias q=exit
alias pcalc="python -i -c 'from math import *' "
# alias diff="$(type colordiff >/dev/null 2>&1 && test $TERM != dumb && echo color)diff -u"
# type trash >/dev/null 2>&1 && alias rm=trash

bak(){
    for file in "$@"
    do
        cp ${file} ${file}.bak
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
memo(){
    _MEMO="# $*\n"
}
rmmemo(){
    _MEMO=""
}
throw-away(){
    for file in "$@"
    do
        mv $file ~/bu/tb
    done
}
mkcd(){
    mkdir $1
    cd $1
}
mkunfddir(){                    # create dir if unfound. ?
    test -e "$1" || mkdir "$1"
}
gitls(){
    for file in `\ls`
    do
        :
    done
}
catclip(){
    if iscygwin
    then
        cat /dev/clipboard | tr -d \\r
    else
        xclip -o -selection "clipboard"
    fi
}
setclip(){
    if iscygwin
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
p(){
    "$@" | $PAGER
}
c(){
    "$@" | cat
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
dl-my-init-files(){
    for file in .bashrc .vimrc .emacs
    do
        local flag=0
        if test -f ~/${file}.new
        then
            mv ~/${file}.new ~/${file}.old
            echo "${file}.new already exist. Rename it to ${file}.old."
            flag=1
        fi
        wget https://dl.dropbox.com/u/1751156/${file} -O ~/${file}.new
        local wgetreturn=$?
        if test ${flag} -eq 1 -a ${wgetreturn} -eq 0
        then
            rm ~/${file}.old
            echo "${file}.old deleted."
        fi
    done
}
port-autosync(){
    port selfupdate && port sync && port upgrade installed
}

_mygitconfig(){
    git config --global user.name "10sr"
    git config --global user.email sr10@users.sourceforge.jp
    git config --global core.autocrlf false
    git config --global color.ui auto
    git config --global status.relativePaths false
    git config --global status.showUntrackedFiles no
    git config --global alias.graph "log --graph --date-order -C -M --pretty=format:\"<%h> %ad [%an] %Cgreen%d%Creset %s\" --all --date=short"
    git config --global alias.st "status -s"
    git config --global alias.b "branch"
    git config --global alias.ci "commit --verbose"
    git config --global alias.co "checkout"
    git config --global alias.cim "commit --verbose -m"
    git config --global alias.di "diff"
    git config --global alias.me "merge --no-ff --stat -v"
    git config --global alias.ls "!git ls-files | xargs ls -CFG --color=auto --time-style=long-iso"
    git config --global alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
    git config --global alias.addi "add -i"
}

__my_parse_svn_branch() {
    local LANG=C
    local svn_url=$(svn info 2>/dev/null | sed -ne 's#^URL: ##p')
    local svn_repository_root=$(svn info 2>/dev/null | sed -ne 's#^Repository Root: ##p')
    echo ${svn_url} | sed -e 's#^'"${svn_repository_root}"'##g' | awk '{print $1}'
}

__my_svn_ps1(){
    local svn_branch=$(__my_parse_svn_branch)
    test "${svn_branch}" == "" || echo ${svn_branch} | xargs printf "$1"
}

prompt_function(){              # used by PS1
    local lastreturn=$?
    if test "${TERM}" == dumb
    then
        local c1=""
        local c2=""
        local c3=""
        local cdef=""
    else
        local c1="\e[33m"
        local c2="\e[36m"
        local c3="\e[37m"
        local cdef="\e[0m"
    fi
    iscygwin || {
        local pwd=$(echo "${PWD}/" | sed -e "s:${HOME}:~:")
        local oldpwd=$(echo "${OLDPWD}/" | sed -e "s:${HOME}:~:")
        local date=$(LANG=C safe-cmd date +"%a, %d %b %Y %T %z")
        local jobnum=$(jobs | wc -l)
    }
    local git=$(safe-cmd __git_ps1 [GIT:%s])
    local svn=$(type svn >/dev/null 2>&1 && safe-cmd __my_svn_ps1 [SVN:%s])
    printf "${_MEMO}"
    printf " [${c1}${pwd}${cdef}<${c3}${oldpwd}${cdef}]${git}${svn}\n"
    printf "${c2}${USER}@${HOSTNAME}${cdef} ${date} ${BASH} ${BASH_VERSION}\n"
    printf "shlv:${SHLVL} jobs:${jobnum} last:${lastreturn} "
}

# type date >/dev/null 2>&1 || alias date=":" # "cmd /c echo %time%"

if [ "${EMACS}" = "t" ]; then   # emacs shell用
    : export PS1="\u@\H \d \t \w\nemacs shell\$ "
elif echo "$EMACS" | grep term >/dev/null 2>&1; then # emacs term用
    echo "emacs term"
fi

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

_echocolors(){
    echo -e \
        "\e[30mBlack\n" \
        "\e[31mRed\n" \
        "\e[32mGreen\n" \
        "\e[33mYellow\n" \
        "\e[34mBlue\n" \
        "\e[35mMagenta\n" \
        "\e[36mCyan\n" \
        "\e[37mWhite\n" \
        "\e[30;1mBright Black\n" \
        "\e[31;1mBright Red\n" \
        "\e[32;1mBright Green\n" \
        "\e[33;1mBright Yellow\n" \
        "\e[34;1mBright Blue\n" \
        "\e[35;1mBright Magenta\n" \
        "\e[36;1mBright Cyan\n" \
        "\e[37;1mBright White\n" \
        "\e[0m"
}

safe-cmd stty stop undef        # unbind C-s to stop displaying output

##########################
# system type
# $OSTYPEとか使えるのかな

iswindows(){
    uname | grep -iE 'windows|MINGW' >/dev/null 2>&1
}

ismsys(){
    false
}

iscygwin(){
    uname | grep -E "^CYGWIN" >/dev/null 2>&1
}

isdarwin(){
    uname | grep -E 'Darwin' >/dev/null 2>&1
}

#########################
# for windose

winln(){
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

########################

if iscygwin; then
    # for cygwin
    export TMP=/tmp
    export TEMP=/tmp
    : alias setclip="tee /dev/clipboard"
    : alias catclip="cat /dev/clipboard | tr -d \\r"
    alias cygsu="cygstart /cygwinsetup.exe"
    alias emacs="CYGWIN=tty emacs"
    echo "cygwin bash"
    export PS1=" [\[\e[33m\]\w\[\e[0m\]]\n\[\e[32m\]\u@\H\[\e[0m\] \d \t \s.\v\nhist:\# jobs:\j \$ "
fi

#######################

echo "Japanese letters are 表示可能"

safe-cmd diskinfo

type xrandr >/dev/null 2>&1 && {
    xrandr | grep --color=never ^Screen
}
safe-cmd finger $USER
LANG=C safe-cmd id

