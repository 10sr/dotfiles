#!/bin/sh
set -e

# setup.sh --- 10sr setup script
# 2014, 10sr. Unlicensed <http://unlicense.org>

__setups="shrc_common gitconf tmux scripts darwin dirs selfupdate windirs dotfiles env"

__homelocal="$HOME/.local"
__homevar="$HOME/.var"
__dotfiles_dir_default="$HOME/10sr_dotfiles"
_current_date=`date`

# TODO: how to give args to command?


#################################################################

_dotfiles_url_base=https://raw.githubusercontent.com/10sr/dotfiles/master/

if test -z "$DOTFILES_DIR"
then
    DOTFILES_DIR="$__dotfiles_dir_default"
fi

ismsys=
iscygwin=
iswindows=

isdarwin=
isfreebsd=
isbsd=

islinux=

###########################
# utils

##########################33
# Utilities

__tput_bold=`tput bold || true`
__tput_default=`tput sgr0 || true`
__tput_yellow=`tput setaf 3 || true`

_msg(){
    echo ">> ${__tput_bold}$*${__tput_default}"
}

_warn(){
    echo ">>> ${__tput_yellow}${__tput_bold}$*${__tput_default}" 1>&2
}

_die(){
    _warn "$@"
    exit 1
}

_download(){
    # download <url> <file>
    if type wget >/dev/null 2>&1
    then
        wget $__my_wget_options "$1" -O "$2"
    elif  type curl >/dev/null 2>&1
    then
        curl --url "$1" --output "$2"
    else
        _warn "No download program found."
        _die "Install wget or curl."
    fi
}

# Detect systems

detect_systems(){
    ismsys=false
    iscygwin=false
    iswindows=false

    isdarwin=false
    isfreebsd=false
    isbsd=false

    islinux=false

    # $OSTYPE is another choice. which is better?
    # NOTE: sh on FreeBSD does not define OSTYPE
    case `uname` in
        (MINGW*) ismsys=true ;;
        (CYGWIN*) iscygwin=true ;;
        (Darwin*) isdarwin=true ;;
        (FreeBSD*) isfreebsd=true ;;
        (Linux*) islinux=true ;;
    esac
    ($ismsys || $iscygwin) && iswindows=true
    # is this true?
    ($isdarwin || $isfreebsd) && isbsd=true
    _msg "Detecting the system...done"
}

################################
# setups


###############################
# setup selfupdate

setup_selfupdate(){
    _msg "Download latest setup.sh from 10sr repository."
    mkdir -p "$DOTFILES_nDIR"
    _download $_dotfiles_url_base/setup.sh "$DOTFILES_DIR/"setup.sh
    chmod +x "$DOTFILES_DIR"/setup.sh
}


##################################
# setup dotfiles

setup_dotfiles(){
    _msg "Prepare latest dotfiles."
    mkdir -p "$DOTFILES_DIR"
    if test "$1" = "--git"
    then
        # git clone
        _msg "Option \"--git\" has been given. Using git"
        if test -d "$DOTFILES_DIR"/.git
        then
            _warn "Git repository $DOTFILES_DIR already exists"
            _warn "Skipping"
        else
            _msg "Checking github.com connectivity"
            ssh git@github.com 2>/dev/null && true
            if test $? -eq 1
            then
                _git_clone_url=git@github.com:10sr/dotfiles.git
                _msg "Authentication succeeded"
            else
                _git_clone_url=https://github.com/10sr/dotfiles.git
                _msg "Authentication failed"
            fi
            _msg "Git cloning $_git_clone_url"
            git clone $_git_clone_url "$DOTFILES_DIR"
        fi
    else
        for f in $@
        do
            _msg "Prepareing $f"
            mkdir -p "`dirname $f`"
            _download $_dotfiles_url_base/$f "$DOTFILES_DIR"/$f
        done
    fi
}

#############################
# setup shrc_common

# Generate ~/.shrc.common, which contains system infos and is sourced from
# setup.sh (this file) and dotfiles/shrc .

# this variable must consistent with shrc
__shrc_common="$HOME/.shrc.common"

setup_shrc_common(){
    _msg "Generate $__shrc_common"
    test -f "$__shrc_common" && rm -- "$__shrc_common"

    cat <<__EOC__ >"$__shrc_common"
#!/bin/sh

# $__shrc_common
# Automatically generated by $0 at $_current_date

ismsys=$ismsys
iscygwin=$iscygwin
iswindows=$iswindows

isdarwin=$isdarwin
isfreebsd=$isfreebsd
isbsd=$isbsd

islinux=$islinux

__homelocal="$__homelocal"
__homevar="$__homevar"
__EOC__
}

################################
# setup gitconf

setup_gitconf(){
    _msg "Configure git environment"
    if ! command -v git >/dev/null
    then
        _msg "Git program not found"
        return 0
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
    $_gc alias.me "merge --no-ff --stat --verbose"
    $_gc alias.gr "grep -n"
    $_gc alias.ls "ls-files"
    # $_gc alias.ls "ls-files -v --full-name"
    # $_gc alias.ls "status -u -s ."
    $_gc alias.sl "!sl"
    # $_gc alias.my-ls "ls-files | xargs ls"
    # $_gc alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
    $_gc alias.addi "add -i"
    $_gc alias.clean-p "!test -z \"\$(git status -s -uno)\""

    # alias open-branch and close-branch, which will be useful for topic branch
    # workflow
    _git_open_branch="checkout -b"
    _git_close_branch="!sh -cx 'git stash && \
git checkout master && git merge --no-ff --stat --verbose -'"
    $_gc alias.open-branch "$_git_open_branch"
    $_gc alias.close-branch "$_git_close_branch"
    $_gc alias.o "$_git_open_branch"
    $_gc alias.c "$_git_close_branch"

    $_gc alias.todo "grep -E -i 'todo:|note:|fixme:'"

    #$_gc alias.wc "!git ls-files -z | xargs -0 wc"
    # $_gc push.default "simple"
    if $iswindows; then
        $_gc core.fileMode false
    fi
}

#############################
# setup tmux

setup_tmux(){
    tmux_conf_local="$HOME/.tmux.conf.local"
    _msg "Generate $tmux_conf_local"

    case "`hostname`" in
        arch-aspireone)
            tmux_bg_color=yellow
            tmux_fg_color=black
            ;;
        arch-mba)
            tmux_bg_color=cyan
            tmux_fg_color=black
            ;;
        newkiwi)
            tmux_bg_color=magenta
            tmux_fg_color=white
            ;;
        freebsd-vb-win7-opti)
            tmux_bg_color=red
            tmux_fg_color=white
            ;;
        *)
            tmux_bg_color=green
            tmux_fg_color=black
            ;;
    esac

    cat <<__EOC__ >"$tmux_conf_local"
# $tmux_conf_local
# Automatically generated by $0 at $_current_date

set -g status-right "${USER}@$(hostname) | #(tmux -V) "

set -g status-bg $tmux_bg_color
set -g status-fg $tmux_fg_color
set -g mode-bg $tmux_bg_color
set -g mode-fg $tmux_fg_color
set -g pane-active-border-fg $tmux_bg_color
__EOC__

    _tmux_conf="$HOME"/.tmux.conf
    if test -f "$_tmux_conf"
    then
        _warn "Tmux config file found. Skipping"
    else
        echo "source \"$DOTFILES_DIR/tmux.conf\"" >>"$_tmux_conf"
    fi
}

##############################
# setup scripts

_fetch_script(){
    # _fetch_script <url> <binname>
    url="$1"
    name="$2"
    dst="$HOME/.local/bin/$name"
    test -f "$dst" && return 0
    command -v "$name" >/dev/null && return 0
    if _download "$url" "$dst"
    then
        chmod u+x "$dst"
    else
        test -f "$dst" && rm -- "$dst"
    fi
}

setup_scripts(){
    _msg "Download some utility scripts"
    _fetch_script \
        https://gist.github.com/10sr/6852317/raw/colortable16.sh colortable16.sh
    _fetch_script \
        https://gist.github.com/10sr/6852331/raw/256colors2.pl 256colors2.pl
    _fetch_script \
        https://github.com/icy/pacapt/raw/ng/pacapt pacapt
}

################################
# setup darwin

__darwin_set_defaults(){
    $isdarwin || return 0

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
    $isdarwin || return 0

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

##########################
# setup windirs

setup_windirs(){
    $iswindows || return 0
    _msg "Setup some directories for windows environment"

    if $iscygwin
    then
        #__winhome="/cygdrive/c/Users/`whoami`"
        __winhome=`cygpath -H`/`whoami`
    fi

    if test -n "$__winhome" -a -d "$__winhome" -a '!' -e "$HOME/.winhome"
    then
        _msg Make symlink to "$__winhome" with name "$HOME/.winhome"
        ln -s "$__winhome" "$HOME/.winhome"
    fi
}

#########################
# setup dirs

setup_dirs(){
    _msg Make some direcoties
    mkdir -vp "$__homelocal"
    mkdir -vp "$__homelocal/bin"
    mkdir -vp "$__homevar"
}


####################################
# setup env
# setup new environment with default options

setup_env(){
    setup_shrc_common
    setup_dirs
    setup_gitconf
    setup_tmux
    setup_scripts
    setup_dotfiles --git
}


#########################
# main

main(){
    detect_systems

    if test -z "$1"
    then
        echo "Usage: ./setup.sh <cmd> ..."
        echo "Available cmds are: $__setups"
        exit 1
    fi

    _cmd=$1
    shift

    _msg Running setup_$_cmd
    setup_$_cmd "$@"
    _msg Running setup_$_cmd done
}

main "$@"
