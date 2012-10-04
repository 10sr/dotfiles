#!/bin/sh

# Wicd - ArchWiki / https://wiki.archlinux.org/index.php/Wicd#Problems_after_package_update
# run this script as root when wicd doesnt work after udpate.

rc.d stop wicd
# rm /etc/wicd/*.conf
mv /etc/wicd/manager-settings.conf /etc/wicd/manager-settings.conf.bak
rc.d start wicd

