#!/bin/sh
set -e

if ! command -v ansible-playbook >/dev/null
then
    echo 'Command not found: ansible-playbook'
    echo 'Install ansible first.'
    exit 1
fi

ansible-playbook -i hosts --ask-sudo-pass "$@"
