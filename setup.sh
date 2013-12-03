#!/bin/sh

# setup.sh --- 10sr setup script

__setups="gitconf tmux scripts darwin dirs selfupdate"

__homelocal="$HOME/.local"
__homevar="$HOME/.var"

###########################
# utils

_download(){
    # download <url> <file>
    if type wget >/dev/null 2>&1
    then
        wget $__my_wget_options "$1" -O "$2"
    elif  type curl >/dev/null 2>&1
    then
        curl --url "$1" --output "$2"
    fi
}


#############################
# gen_common

# Generate ~/.shrc.common, which contains system infos and is sourced from
# setup.sh (this file) and dotfiles/shrc .
# This functions is always called.

# this variable must consistent with shrc
__shrc_common="$HOME/.shrc.common"

gen_common(){
    test -f "$__shrc_common" && rm -- "$__shrc_common"

    __ismsys=false
    __iscygwin=false
    __iswindows=false
    __isdarwin=false
    __islinux=false
    # $OSTYPE is another choice. which is better?
    case `uname` in
        (MINGW*) __ismsys=true ;;
        (CYGWIN*) __iscygwin=true ;;
        (Darwin*) __isdarwin=true ;;
        (Linux*) __islinux=true ;;
    esac
    ( $__ismsys || $__iscygwin ) && __iswindows=true

    cat <<__EOC__ >"$__shrc_common"
#!/bin/sh

# $__shrc_common
# Automatically generated from $0

ismsys=$__ismsys
iscygwin=$__iscygwin
iswindows=$__iswindows
isdarwin=$__isdarwin
islinux=$__islinux

__homelocal="$__homelocal"
__homevar="$__homevar"
__EOC__
}

###############################
# selfupdate

__setup_url="https://raw.github.com/10sr/dotfiles/master/setup.sh"

setup_selfupdate(){
    if test -z "$SETUP_OUTPUT"
    then
        echo SETUP_OUTPUT is not set.
        echo Ignore selfupdate.
        return
    fi
    _download $__setup_url "$SETUP_OUTPUT"
}

################################
# git_configs

setup_gitconf(){
    if ! command -v git >/dev/null
    then
        echo "git not found"
        return 1
    fi

    _gc="git config --global"

    $_gc user.name '10sr'
    $_gc user.email '8slashes+git@gmail.com'
    $_gc core.autocrlf false
    $_gc core.excludesfile '~/.gitignore'
    $_gc color.ui auto
    $_gc status.relativePaths false
    $_gc status.showUntrackedFiles normal
    $_gc log.date iso
    command -v xz >/dev/null && \
        $_gc tar.txz.command "xz -c"
    $_gc push.default current

    $_gc alias.graph "log --graph --date-order -C -M --pretty=tformat:\"%C(green)%h%C(reset) %C(white)%ad%C(reset) %C(red)%an%C(reset)%C(yellow)%d%C(reset) %C(white bold)%s%C(reset)\" --all --date=iso -n 499"
    $_gc alias.st "status -s -b"
    $_gc alias.b "branch"
    $_gc alias.sb "show-branch"
    $_gc alias.ci "commit --verbose"
    $_gc alias.co "checkout"
    $_gc alias.cim "commit --verbose -m"
    $_gc alias.di "diff --color"
    $_gc alias.me "merge --no-ff --stat -v"
    $_gc alias.gr "grep -n"
    $_gc alias.ls "ls-files"
    # $_gc alias.ls "ls-files -v --full-name"
    # $_gc alias.ls "status -u -s ."
    $_gc alias.sl "!sl"
    # $_gc alias.my-ls "ls-files | xargs ls"
    # $_gc alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
    $_gc alias.addi "add -i"
    $_gc alias.clean-p "!test -z \"\$(git status -s -uno)\""
    $_gc alias.bopen "checkout -b"
    $_gc alias.bclose \
        "!sh -cx 'git stash && git checkout master && git merge --no-ff -'"
    #$_gc alias.wc "!git ls-files -z | xargs -0 wc"
    # $_gc push.default "simple"
    if $iswindows; then
        $_gc core.fileMode false
    fi
}

#############################
# gen_tmux_conf_local

setup_tmux(){
    tmux_conf_local="$HOME/.tmux.conf.local"

    case "`hostname`" in
        arch-aspireone)
            tmux_bg_color=blue
            tmux_fg_color=white
            ;;
        darwin-mba.local)
            tmux_bg_color=cyan
            tmux_fg_color=black
            ;;
        newkiwi)
            tmux_bg_color=magenta
            tmux_fg_color=white
            ;;
        *)
            tmux_bg_color=green
            tmux_fg_color=black
            ;;
    esac

    cat <<__EOC__ >"$tmux_conf_local"
# $tmux_conf_local
# Automatically generated from $0

set -g status-right "${USER}@$(hostname) | #(tmux -V) "

set -g status-bg $tmux_bg_color
set -g status-fg $tmux_fg_color
set -g mode-bg $tmux_bg_color
set -g mode-fg $tmux_fg_color
set -g pane-active-border-fg $tmux_bg_color
__EOC__
}

##############################
# install_scripts

_fetch_script(){
    # _fetch_script <url> <binname>
    url="$1"
    name="$2"
    dst="$HOME/.local/bin/$name"
    command -v "$name" >/dev/null && return
    if __download "$url" "$dst"
    then
        chmod u+x "$dst"
    else
        test -f "$dst" && rm -- "$dst"
    fi
}

setup_scripts(){
    _fetch_script \
        https://gist.github.com/10sr/6852317/raw/colortable16.sh colortable16.sh
    _fetch_script \
        https://gist.github.com/10sr/6852331/raw/256colors2.pl 256colors2.pl
}

################################
# darwin

__darwin_set_defaults(){
    $isdarwin || return 1

    # http://appdrill.net/60641/mac-boot-mute.html
    #sudo nvram SystemAudioVolume=%80

    # add quit entry in menu
    defaults write com.apple.finder QuitMenuItem -bool YES
    # show full path on titlebar
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
    # do not show desktop icons
    defaults write com.apple.finder CreateDesktop -boolean false

    killall Finder

    # disable dashboard
    #defaults write com.apple.dashboard mcx-disabled -bool YES
}

__darwin_start_daemon(){
    $isdarwin || return 1

    test "`launchctl getenv LC_ALL`" = C || sudo launchctl setenv LC_ALL C
    if ! (launchctl list | grep com.apple.locate) >/dev/null
    then
        sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
    fi
}

setup_darwin(){
    __darwin_set_defaults
    __darwin_start_daemon
}

#########################
# mkdirs

setup_dirs(){
    install -d "$__homelocal"
    install -d "$__homelocal/bin"
    install -d "$__homevar"
}

#########################
# main

main(){

    gen_common
    . "$__shrc_common"

    if test -z "$1"
    then
        echo "usage: ./setup.sh <setups> ..."
        echo "setups: all $__setups"
        exit 1
    fi

    while test -n "$1"
    do

        if test "$1" = all
        then
            for c in $__setups
            do
                set -x
                setup_$c
                set +x
            done
        fi

        for c in $__setups
        do
            if test "$1" = "$c"
            then
                set -x
                setup_$c
                set +x
            fi
        done

        shift
    done
}

main "$@"
