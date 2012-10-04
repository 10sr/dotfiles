port echo requested >port.lst

for f in /opt/local/etc/macports/variants.conf
do
    test -r $f && cp -vu $f .
done


