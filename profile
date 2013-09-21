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

#_python_pkg_conf="/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/pkgconfig"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$_python_pkg_conf"

#export MANPATH="`manpath`:$HOME/.local/share/man"

# in my environment powerdown does not work
test -z "$SSH_CONNECTION" && \
    type setterm >/dev/null 2>&1 && setterm -blank 30 -powersave on # -powerdown 10
