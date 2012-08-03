#!/bin/sh

mkdir -p ~/.my/log
mkdir -p ~/.local/bin

_my_install_script(){
    local dir="$HOME/.local/bin"
    mkdir -p "$dir"
    for f in "$@"
    do
        bn=$(basename "$f")
        type $bn >/dev/null 2>&1 || {
            if type wget >/dev/null 2>&1
            then
                wget "$f" -P "$dir/" &&
                chmod u+x "${dir}/${bn}"
            elif  type curl >/dev/null 2>&1
            then
                curl --url "$f" --output "${dir}/${bn}" &&
                chmod u+x "${dir}/${bn}"
            fi
        }
    done
}
_my_install_script http://www.frexx.de/xterm-256-notes/data/colortable16.sh http://www.frexx.de/xterm-256-notes/data/256colors2.pl

_my_install_symlink_script(){
    mkdir -p "$HOME/.local/bin/"
    for f in "$@"
    do
        ln -s "$PWD/$f" "$HOME/.local/bin/"
    done
}
