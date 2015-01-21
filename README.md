[![Build Status](https://travis-ci.org/10sr/dotfiles.svg?branch=master)](https://travis-ci.org/10sr/dotfiles)

Dotfiles
========



First Principle
---------------


__Do not go out of ways how the default environments work__.


There are many cases when I must work on foreign environments, and it is always
VERY irritating when things do not work as I expected, yet setting up my own
environment is troublesome. So I avoid changing behaviors of tools, except
for emacs.

Emacs is the editor I usually use for coding. I decided to change the behavior
of emacs as I want without thinking about other environments. When I cannot use
my emacs.el I'll use vi or vim instead.

Other tools like bash and vim basically work in the same ways even when using my
rc files. The main changes are related to showing informations, typically in the
bash prompt.



shrc.common
-----------

`setup.sh` will create file "$HOME/.shrc.common". This file define some
variables that can be used to detect system type. This file is sourced from
both `setup.sh` and `shrc`.



Sourcing flow
-------------

1. $HOME/.profile

  Sourced if:

  * currnet shell is login shell and
    * (current shell is sh ||
    * (current shell is bash && $HOME/.bash_profile does not exist) ||
    * (current shell is zsh && $HOME/.zprofile does not exist))

  This file set ENV to $HOME/.shrc , which is loaded when sh is run interactively.
  Add lines like `test -n "$BASH_VERSION" && . "$HOME/.bashrc"` .



2. $HOME/{.bash_profile,.zprofile}

  Sourced if current shell is login shell and shell is bash or zsh.
  Srouce $HOME/.profile .


3. $HOME/.shrc

  Sourced through ENV when shell is sh, and $HOME/.{ba,z}shrc if bash or zsh
  respectively. Source $HOME/.dotfiles/shrc .


4. $HOME/.{ba,z}shrc

  Sourced when current shell is bash or zsh respectively and current shell is not
  login shell. When current shell is login shell, these files are sourced
  explicitly by $HOME/.profile . Source $HOME/.shrc .


```
$HOME/{.bash_profile,.zprofile}  <= login with bash/zsh
|
`--$HOME/.profile                <= login with sh
   |  |
   |  `--$HOME/.dotfiles/profile
   |
   |  $HOME/.{ba,z}shrc          <= bash, zsh
   |  |
   `--`--$HOME/.shrc             <= sh (by EnvVal ENV)
         |
         `--$HOME/.dotfiles/shrc
```

```
~/.xinitrc                <= startx
|
`--openbox-session
   |
   |--~/.dotfiles/xprofile
   |
   `--~/.dotfiles/xprograms
```


Get Latest Makefile
-------------------

NOTE: Currently on the way of migration from `setup.sh` to `Makefile`.
`setup.sh` is available at `j.mp/10sr_setup`.

Issue

    curl -L j.mp/10sr_make | make -f -

to use latest Makefile.
