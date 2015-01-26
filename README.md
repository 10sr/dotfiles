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



Get Latest Makefile
-------------------

Issue

    curl -L j.mp/10sr_make | make -f -

to use latest Makefile.
