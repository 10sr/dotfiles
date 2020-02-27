#!/bin/sh
set -eux

dir=$HOME/.config/karabiner/assets/complex_modifications

mkdir -p $dir

for file in terminal-disable.json
do
    cp -vpf $file $dir
done
