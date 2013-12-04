#!/usr/bin/python

# Example usage.
# Place the following in file called post-checkout in .git/hooks/
#
#     #!/bin/bash
#     /Users/zaking/.emacs.d/etags.sh -py -m -js -!/env/
#
# Ignores the /env/ directory anywhere in the path.

from sys import argv
from subprocess import call

types = "|".join([a.strip('-') for a in argv[1:] if not "!" in a])
ignore = " ".join(['\! -path "*%s*"' % a.strip('-!')  for a in argv[1:] if "!" in a])

cmd = 'rm TAGS;find -E . %s \! -path "*/.*" -regex ".*\.(%s)$" -exec etags -a {} \;' % (ignore, types)

status = call(cmd, shell=True)
print "generated TAGS file"
