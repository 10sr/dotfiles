#!/bin/sh

mkdir -p ~/.my/log
mkdir -p ~/.local/bin

gen_source_script(){
    # _gen_source_script file lines
    test $# -eq 2 || return 1
    head -n $2 $1 | \grep -v '^#!' | sed -e 's/^..//g'
}

get_install_script(){
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

install_symlink_script(){
    mkdir -p "$HOME/.local/bin/"
    for f in "$@"
    do
        ln -s "$PWD/$f" "$HOME/.local/bin/"
    done
}

git_config(){
    type git >/dev/null 2>&1 || return 1

    git config --global user.name '10sr'
    git config --global user.email '8slashes+git@gmail.com'
    git config --global core.autocrlf false
    git config --global core.excludesfile '~/.gitignore'
    git config --global color.ui auto
    git config --global status.relativePaths false
    git config --global status.showUntrackedFiles normal
    git config --global log.date iso
    git config --global alias.graph "log --graph --date-order -C -M --pretty=tformat:\"<%h> %ad [%an] %Cgreen%d%Creset %s\" --all --date=iso"
    git config --global alias.st "status -s -b"
    git config --global alias.b "branch"
    git config --global alias.ci "commit --verbose"
    git config --global alias.co "checkout"
    git config --global alias.cim "commit --verbose -m"
    git config --global alias.di "diff --color"
    git config --global alias.me "merge --no-ff --stat -v"
    git config --global alias.ls "ls-files -v --full-name"
    git config --global alias.sl "!sl"
    # git config --global alias.my-ls "ls-files | xargs ls"
    # git config --global alias.ll "!git ls-files | xargs ls -l -CFG --color=auto --time-style=long-iso"
    git config --global alias.addi "add -i"
    if false iswindows; then
        git config --global core.fileMode false
    fi
}

mac_defaults(){
    test "`uname`" = Darwin || return 1

    # add quit entry in menu
    defaults write com.apple.finder QuitMenuItem -bool YES
    # show full path on titlebar
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
    # do not show desktop icons
    defaults write com.apple.finder CreateDesktop -boolean false

    killall finder

    # disable dashboard
    #defaults write com.apple.dashboard mcx-disabled -bool YES
}

get_install_script http://www.frexx.de/xterm-256-notes/data/colortable16.sh http://www.frexx.de/xterm-256-notes/data/256colors2.pl

git_config

mac_defaults
