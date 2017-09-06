#!/bin/sh
# -*- coding: utf-8 -*-

set -eu

verbosity=""
mbox="-a"

while true; do
    case "${1:-}" in
    --loop) loop=1; shift;;
    --quick) mbox="INBOX"; shift;;
    -V) verbosity="-V"; shift;;
    -q) verbosity="-q"; shift;;
    *)  break ;;
    esac
done

sync() {
    mbsync $verbosity $mbox && echo OK
    mu index -q --maildir=~/Mail
    mu find --clearlinks --format=links --linksdir=~/Mail/INBOX \
        m:'/cral/INBOX' OR m:'/sconseil/INBOX' OR m:'/saimon/INBOX'
}


if [ ${loop:-0} -eq 1 ]; then
    while true;
    do
        echo -n "`date +%H:%M` Syncing... "
        sync
        sleep 5m
    done
else
    sync
fi

exit
