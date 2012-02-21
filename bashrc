#!/bin/bash

test -r /etc/bashrc && . /etc/bashrc

##########################
# system type

if uname | grep -E "^MINGW32" >/dev/null 2>&1
then
    alias ismsys=true
else
    alias ismsys=false
fi

if uname | grep -E "^CYGWIN" >/dev/null 2>&1
then
    alias iscygwin=true
else
    alias iscygwin=false
fi

alias iswindows="iscygwin || ismsys"

if uname | grep -E 'Darwin' >/dev/null 2>&1
then
    alias isdarwin=true
else
    alias isdarwin=false
fi

##########################################
null(){
    "$@" >/dev/null 2>&1
}
__try_exec(){
    type $1 >/dev/null 2>&1 && "$@"
}

export PS1="\$(__my_prompt_function)\$ "
# PROMPT_COMMAND=prompt_function
if false # iswindows
then
    export PAGER='tr -d \\r | less'
else
    export PAGER="less"
fi

if type vim >/dev/null 2>&1
then
    export EDITOR=vim
else
    export EDITOR=vi
fi
export VISUAL="$EDITOR"
export LESS="-iRMX"
# export LC_MESSAGES="C"
# export LANG=ja_JP.UTF-8
# export CDPATH=".:~"             # 使い方がよく分からない
export GIT_PAGER="$PAGER"
export GIT_EDITOR="$EDITOR"

alias ls="ls -hCFG $(test "$TERM" == dumb || echo --color=auto\ )--time-style=long-iso"
# alias ll="ls -l"
# alias la="ls -A"
# alias lla="ls -Al"
# alias less=""
alias vl=/usr/share/vim/vimcurrent/macros/less.sh
alias em="emacs -nw"
# alias apt-get="sudo apt-get"
alias aptin="apt-get install"
alias aptsearch="apt-cache search"
alias aptshow="apt-cache show"
alias ut="ssh t110414@un001.ecc.u-tokyo.ac.jp"
alias rand="echo \$RANDOM"
alias xunp="file-roller -h"
alias pacome="sudo \paco -D"
alias psall="ps auxww"
alias g=git
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
if isdarwin
then
    alias upgrade="port selfupdate && port sync && port upgrade installed"
else
    alias upgrade="sudo apt-get autoremove --yes && sudo apt-get update --yes && sudo apt-get upgrade --yes"
fi
# alias diff="$(type colordiff >/dev/null 2>&1 && test $TERM != dumb && echo color)diff -u"
# type trash >/dev/null 2>&1 && alias rm=trash

export __MYGITBAREREP="${HOME}/dbx/.git-bare"
git-make-local-rep(){
    test $# -eq 0 && {
        echo "specify repository name." 1>&2
        return 1
    }

    dir="${__MYGITBAREREP}/$1.git"

    if test -d "$dir"
    then
        echo "dir $dir already exist!" 1>&2
    else
        mkdir -p "$dir" && {
            pushd "$dir" &&
            git init --bare --shared=all
            popd
        }
    fi
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
    mkdir -p ~/bu/tb
    for file in "$@"
    do
        mv $file ~/bu/tb
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
    git config --global alias.di "diff"
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

if type _git >/dev/null 2>&1    # enable programmable completion of g
then
    complete -o bashdefault -o default -o nospace -F _git g 2>/dev/null \
	|| complete -o default -o nospace -F _git g
fi

__my_parse_svn_branch() {
    local LANG=C
    local svn_url=$(svn info 2>/dev/null | sed -ne 's#^URL: ##p')
    local svn_repository_root=$(svn info 2>/dev/null | sed -ne 's#^Repository Root: ##p')
    echo ${svn_url} | sed -e 's#^'"${svn_repository_root}"'##g' | awk '{print $1}'
}

__my_svn_ps1(){
    local svn_branch=$(__my_parse_svn_branch)
    test -n "${svn_branch}" && printf "$1" "{$svn_branch}"
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
        local jobnum=
        if git branch >/dev/null 2>&1
        then
            local git="[GIT]"
        else
            local git=
        fi
        local date=$(/c/Windows/System32/cmd.exe //c 'echo %DATE%-%TIME%')
        :
    else
        local pwd=$(echo "${PWD}/" | sed -e "s#${HOME}#~#")
        local oldpwd=$(echo "${OLDPWD}/" | sed -e "s#${HOME}#~#")
        local jobnum=$(jobs | wc -l)
        local git=$(__try_exec __git_ps1 [GIT:%s])
        local date=$(LANG=C __try_exec date +"%a, %d %b %Y %T %z")
    fi
    local svn=$(type svn >/dev/null 2>&1 && __try_exec __my_svn_ps1 [SVN:%s])
    jobs
    printf " [${c1}${pwd}${cdef}<${c3}${oldpwd}${cdef}]${git}${svn}\n"
    printf "${c2}${USER}@${HOSTNAME}${cdef} ${date} ${BASH} ${BASH_VERSION}\n"
    printf "shlv:${SHLVL} jobs:${jobnum} last:${lastreturn} "
}

# type date >/dev/null 2>&1 || alias date=":" # "cmd /c echo %time%"

if [ "${EMACS}" = "t" ]; then   # emacs shell用
    true export PS1="\u@\H \d \t \w\nemacs shell\$ "
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

null type stty && {
    stty stop undef        # unbind C-s to stop displaying output
    stty erase '^h'
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
    alias cygsu="cygstart /cygwinsetup.exe"
    alias emacs="CYGWIN=tty emacs -nw"
    echo "cygwin bash"
fi

if iswindows; then
    # export TMP=/tmp
    # export TEMP=/tmp
    # export PS1=" \[\e[32m\]\u@\H \[\e[33m\]\w\[\e[0m\] \d \t\n\s \# \j \$ "
    # export PS1=" [\[\e[33m\]\w\[\e[0m\]]\n\[\e[32m\]\u@\H\[\e[0m\] \d \t \s.\v\nhist:\# jobs:\j \$ "
    alias ls="ls -CFG $(test "$TERM" == dumb || echo --color=auto)"
    export USER=$USERNAME
fi

#######################

uname -a
test -f /etc/issue.net && cat /etc/issue.net

showinfo(){
    echo "Japanese letters are 表示可能"

    __try_exec diskinfo

    ! isdarwin && test -n "${DESKTOP_SESSION}" && type xrandr >/dev/null 2>&1 && {
        xrandr | grep --color=never ^Screen
    }

    iswindows || __try_exec finger $USER
    LANG=C __try_exec id

}
