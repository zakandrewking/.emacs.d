#!/bin/bash
# make a .app with Platypus for Mac OS X
# see: http://bc.tech.coop/blog/070225.html
/usr/local/bin/emacsclient -n "${1}" 2> /dev/null
if [ $? -ne 0 ]; then
    open -a /Applications/Aquamacs.app "${1}"
fi