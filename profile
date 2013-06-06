#!/bin/sh

# # sample of ~/.profile

# . ~/.dotfiles/profile

# # if running bash
# if [ -n "$BASH_VERSION" ]; then
#     # include .bashrc if it exists
#     if [ -f "$HOME/.bashrc" ]; then
# 	    . "$HOME/.bashrc"
#     fi
# fi

# This not works when, for example, inherit parent ENV and load /etc/profile
# again.
#test -n "$DOTFILES_PROFILE" && return
#export DOTFILES_PROFILE=t

# export PS1="\$ "
export LC_TIME=C
export TERMCAP="${TERMCAP}:vb="
export HOSTNAME
export ENV=~/.shrc

mkdir -p $HOME/.local/lib/python/site-packages
export PYTHONDOCS=/usr/share/doc/python/html/
export PYTHONPATH="${PYTHONPATH}:\
${HOME}/my/bin/py:${HOME}/.local/lib/python/site-packages"
test -f "${HOME}/.dotfiles/rc.py" && \
    export PYTHONSTARTUP="${HOME}/.dotfiles/rc.py"
#export PYTHONPATH="~/.local/share/lib/python3.2/site-packages"

export GEM_HOME="$HOME/.local/lib/gems"
export PATH="$PATH:$HOME/.local/lib/gems/bin"
export RUBYLIB="$RUBYLIB:$HOME/.local/lib/gems/lib"

__add_to_path(){
    for p in "$@"
    do
        echo $PATH | grep -E "^$p:|:$p:|:$p$" >/dev/null 2>&1 || \
            PATH="$p:${PATH}"
    done
}
# export PATH="${PATH}:${HOME}/bin"
__add_to_path ${HOME}/.local/bin /c/mingw/bin /c/mingw/msys/1.0/bin

# # it is not so good
# # http://archive.linux.or.jp/JF/JFdocs/Program-Library-HOWTO/shared-libraries.html
# # http://superuser.com/questions/324613/installing-a-library-locally-in-home-directory-but-program-doesnt-recognize-it
# without this ENV i cannot run tmux. another way is to use --disable-shared
# when building tmux
export LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
# this should be used when ./configure
#export CFLAGS="$CFLAGS -I$HOME/.local/include"

#_python_pkg_conf="/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/pkgconfig"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$_python_pkg_conf"

export MANPATH="$MANPATH:$HOME/.local/share/man"

type setterm >/dev/null 2>&1 && setterm -blank 30 -powersave on # -powerdown 10
# in my environment powerdown does not work

if test -n "$TMPDIR"
then
    export TMP=$TMPDIR
else
    export TMP=/tmp/
fi
export TMP="${TMP}${USER}-tmp"
export TEMP="$TMP"
mkdir -p "$TMP"
