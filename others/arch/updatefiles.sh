#!/bin/sh
# copy some important files into current directory.

files="
/etc/rc.conf
/var/log/pacman.log
/etc/pacman.d/gnupg/gpg.conf
/etc/vconsole.conf
"

convpass(){
    echo $1 | sed -e 's / ! g'
}

for i in $files
do
    cp -fvu $i ./$(basename $i)
done

mapdir=/usr/share/kbd/keymaps/i386/qwerty

test ${mapdir}/myjp106.map.gz -nt ./myjp106.map &&
zcat ${mapdir}/myjp106.map.gz >./myjp106.map &&
{
    test -f ./jp106.map || 
    zcat ${mapdir}/jp106.map.gz >./jp106.map

    diff -u ./jp106.map ./myjp106.map >mymap.diff
    echo "Make myjp106 patch."
}

pacman -Qqe | grep -vx "$(pacman -Qqm)" > ./pkg.lst &&
echo "Make pkg.lst."
