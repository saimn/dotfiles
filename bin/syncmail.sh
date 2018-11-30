#!/bin/sh
# -*- coding: utf-8 -*-

set -eu

verbosity=""
mbox="-a"
delay="5m"

while true; do
    case "${1:-}" in
    --loop) loop=1; shift;;
    --delay) shift; delay=$1; shift;;
    --quick) mbox="INBOX"; shift;;
    -V) verbosity="-V"; shift;;
    -q) verbosity="-q"; shift;;
    *)  break ;;
    esac
done

sync() {
    mbsync $verbosity $mbox
    mu index -q --maildir=~/Mail
    # mu find --clearlinks --format=links --linksdir=~/Mail/INBOX \
    #     m:'/cral/INBOX' OR m:'/sconseil/INBOX' OR m:'/saimon/INBOX'
}


if [ ${loop:-0} -eq 1 ]; then
    while true;
    do
        echo -n "`date +%H:%M` Syncing... "
        sync
        echo "Done, $(date +%H:%M), sleeping for $delay"
        sleep $delay
    done
else
    sync
fi

exit
