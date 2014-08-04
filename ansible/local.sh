#!/bin/sh
set -e

if ! command -v ansible-playbook >/dev/null
then
    echo 'Command not found: ansible-playbook'
    echo 'Install ansible first.'
    exit 1
fi

if test $# -eq 0
then
    echo "usage: $0 <playbook> [...]"
    exit 1
fi

ansible-playbook -i local.hosts --ask-sudo-pass "$@"
