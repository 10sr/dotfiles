#!/bin/sh

# setup.sh --- 10sr setup script

__homelocal="$HOME/.local"
__homevar="$HOME/.var"

#############################
# gen_common

# Generate ~/.shrc.common, which contains system infos and sourced from
# setup.sh (this file) and dotfiles/shrc .

# this variable must consistent with shrc
__shrc_common="$HOME/.shrc.common"

gen_common(){
    test -f "$__shrc_common" && rm -- "$__shrc_common"

    __ismsys=false
    __iscygwin=false
    __iswindows=false
    __isdarwin=false
    __islinux=false
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

alias ismsys=$__ismsys
alias iscygwin=$__iscygwin
alias iswindows=$__iswindows
alias isdarwin=$__isdarwin
alias islinux=$__islinux

__homelocal="$__homelocal"
__homevar="$__homevar"
__EOC__
}

##############################
# install_scripts

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

install_scripts(){
    _fetch_script \
        https://gist.github.com/10sr/6852317/raw/colortable16.sh colortable16.sh
    _fetch_script \
        https://gist.github.com/10sr/6852331/raw/256colors2.pl 256colors2.pl
}

################################
# darwin_set_defaults

darwin_set_defaults(){
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

################################
# darwin_start_daemon

darwin_start_daemon(){
    $isdarwin || return 1

    test "`launchctl getenv LC_ALL`" = C || sudo launchctl setenv LC_ALL C
    if ! (launchctl list | grep com.apple.locate) >/dev/null
    then
        sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
    fi
}

################################
# git_configs

git_configs(){
    if ! command -v git >/dev/null
    then
        echo "git not found"
        return 1
    fi

    _gitconfig="git config --global"

    $_gitconfig user.name '10sr'
    $_gitconfig user.email '8slashes+git@gmail.com'
    $_gitconfig core.autocrlf false
    $_gitconfig core.excludesfile '~/.gitignore'
    $_gitconfig color.ui auto
    $_gitconfig status.relativePaths false
    $_gitconfig status.showUntrackedFiles normal
    $_gitconfig log.date iso
    command -v xz >/dev/null && \
        $_gitconfig tar.txz.command "xz -c"
    $_gitconfig push.default current

    $_gitconfig alias.graph "log --graph --date-order -C -M --pretty=tformat:\"%C(green)%h%C(reset) %C(white)%ad%C(reset) %C(red)%an%C(reset)%C(yellow)%d%C(reset) %C(white bold)%s%C(reset)\" --all --date=iso -n 499"
    $_gitconfig alias.st "status -s -b"
    $_gitconfig alias.b "branch"
    $_gitconfig alias.sb "show-branch"
    $_gitconfig alias.ci "commit --verbose"
    $_gitconfig alias.co "checkout"
    $_gitconfig alias.cim "commit --verbose -m"
    $_gitconfig alias.di "diff --color"
    $_gitconfig alias.me "merge --no-ff --stat -v"
    $_gitconfig alias.gr "grep -n"
    $_gitconfig alias.ls "ls-files"
    # $_gitconfig alias.ls "ls-files -v --full-name"
    # $_gitconfig alias.ls "status -u -s ."
    $_gitconfig alias.sl "!sl"
    # $_gitconfig alias.my-ls "ls-files | xargs ls"
    # $_gitconfig alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
    $_gitconfig alias.addi "add -i"
    $_gitconfig alias.clean-p "!test -z \"\$(git status -s -uno)\""
    $_gitconfig alias.bopen "checkout -b"
    $_gitconfig alias.bclose \
        "!sh -cx 'git stash && git checkout master && git merge --no-ff -'"
    #$_gitconfig alias.wc "!git ls-files -z | xargs -0 wc"
    # $_gitconfig push.default "simple"
    if $iswindows; then
        $_gitconfig core.fileMode false
    fi
}

#########################
# mkdirs

mkdirs(){
    install -d "$__homelocal"
    install -d "$__homelocal/bin"
    install -d "$__homevar"
}

#########################
# main

main(){
    set -x

    gen_common
    . "$__shrc_common"

    mkdirs
    install_scripts
    git_configs
    if $isdarwin
    then
        darwin_set_defaults
        darwin_start_daemon
    fi

    set +x
}

main "$@"
