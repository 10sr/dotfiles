#!/bin/sh

ansible-playbook -i hosts ansible.yml --ask-sudo-pass
