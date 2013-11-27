#!/usr/bin/python

# Example usage.
# Place the following in file called post-checkout in .git/hooks/
#
#     #!/bin/bash
#     /Users/zaking/.emacs.d/etags.sh -py -m -js

from sys import argv
from subprocess import call

types = "|".join([a.strip('-') for a in argv[1:]])
    
cmd = 'find -E . -regex ".*\.(%s)$" -exec etags -a {} \;' % types

status = call(cmd, shell=True)
print "generated TAGS file"
