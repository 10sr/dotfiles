#!/bin/sh

# profile --- 10sr profile

__safe_add_path_r(){
    # add path to right
    test -d "$1" && PATH="${PATH}:$1"
}
__safe_add_path_l(){
    # add path to left
    test -d "$1" && PATH="$1:${PATH}"
}

__safe_add_path_l "$HOME/.cabal/bin"
__safe_add_path_l "$HOME/.local/lib/gems/bin"
__safe_add_path_l "$HOME/.gem/ruby/2.1.0/bin"
__safe_add_path_l "$HOME/.local/bin"
__safe_add_path_r "/c/mingw/bin"
__safe_add_path_r "/c/mingw/msys/1.0/bin"

export GEM_HOME="$HOME/.local/lib/gems"
export RUBYLIB="$RUBYLIB:$HOME/.local/lib/gems/lib"


# in my environment powerdown does not work
test -z "$DISPLAY" && test -z "$SSH_CONNECTION" && \
    type setterm >/dev/null 2>&1 && \
    setterm -blank 30 -powersave on # -powerdown 10

##################################
# EnvVal definitions

test "$TERM" = linux && export LANG=C
export LC_MESSAGES=C
export LC_TIME=C

export TERMCAP="${TERMCAP}:vb="
# export ENV=~/.shrc

export PAGER="less"
export LESS="-iMX"

# Style for lesspipe is defined in esc.style
_src_hilite_lp_path="`command -v src-hilite-lesspipe.sh 2>/dev/null`"
for f in /usr/share/source-highlight/src-hilite-lesspipe.sh
do
    test -z "$_src_hilite_lp_path" && test -e "$f" && _src_hilite_lp_path="$f"
done
test -n "$_src_hilite_lp_path" && export LESSOPEN="| $_src_hilite_lp_path %s"

if false which nvim >/dev/null
then
    _VI_ALT=nvim
elif which vim >/dev/null
then
    _VI_ALT=vim
elif which vi >/dev/null
then
    _VI_ALT=vi
fi

if test -n "$_VI_ALT"
then
    export EDITOR="$_VI_ALT"
    export GIT_EDITOR="$EDITOR"
    export VISUAL="$EDITOR"
fi

# export CDPATH=".:~"
export GIT_PAGER="less -FX"
export GIT_MERGE_AUTOEDIT=no

if test -n "$TMUX" && \
        expr "$TERM" : screen >/dev/null && \
        expr "`tmux display -p '#{client_termname}'`" : '.*-256color$' >/dev/null
then
    TERM=screen-256color
fi

if test -z "$USER" -a -n "$USERNAME"
then
    export USER=$USERNAME
fi

# set TMP, TEMP, TMPDIR

if test -z "$TMP"
then
    if test -n "$TMPDIR"
    then
        export TMP=$TMPDIR
    elif test -n "$TEMP"
    then
        export TMP="$TEMP"
    else
        export TMP=/tmp
    fi
fi

expr "$TMP" : ".*$USER-tmp" >/dev/null || export TMP="${TMP}/${USER}-tmp"
export TEMP=$TMP
export TMPDIR=$TMP
test -d "$TMP" || mkdir -p "$TMP"

if test -d ~/dbx
then
    export CHIT_PATH="$HOME/dbx/.chit"
fi


export JAVA_OPTS="$JAVA_OPTS -Duser.language=en"

export GOPATH="$HOME/my/go"
export PATH="$GOPATH/bin:$PATH"

export PIPENV_VENV_IN_PROJECT=1

###########################################
# Host colors

export _HOSTCOLOR_1=
export _HOSTCOLOR_2=
# black red green yellow blue magenta cyan white
# Yellow is hard to read strings...
case "`hostname`" in
    arch-vb-win8-vaio11)
        _HOSTCOLOR_1=magenta
        _HOSTCOLOR_2=white
        ;;
    darwin-mba.local)
        _HOSTCOLOR_1=cyan
        _HOSTCOLOR_2=black
        ;;
    newkiwi)
        _HOSTCOLOR_1=magenta
        _HOSTCOLOR_2=white
        ;;
    debian-vb-win7-opti)
        _HOSTCOLOR_1=red
        _HOSTCOLOR_2=white
        ;;
    *)
        _HOSTCOLOR_1=green
        _HOSTCOLOR_2=black
        ;;
esac
