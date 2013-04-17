#!/bin/bash
# make a .app with Platypus for Mac OS X
/usr/local/bin/emacsclient -n "${1}" 2> /dev/null
# if [ $? -ne 0 ]; then
#    open -a /Applications/Aquamacs\ Emacs.app "${2}"
# fi