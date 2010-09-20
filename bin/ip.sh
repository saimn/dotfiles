#!/bin/sh
# usage: IP Publique ${execi 1800 ~/bin/ip.sh}

wget http://checkip.dyndns.org/ -O - -o /dev/null | cut -d: -f 2 | cut -d\< -f 1
