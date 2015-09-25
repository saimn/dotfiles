#!/bin/bash
# -*- coding: utf-8 -*-

while true;
do
    echo -n "`date +%H:%M` Syncing... "
    mbsync -q -a && echo OK
    mu index -q --maildir=~/Mail
    mu find --clearlinks --format=links --linksdir=~/Mail/INBOX \
        m:'/cral/INBOX' OR m:'/sconseil/INBOX' OR m:'/saimon/INBOX'
    sleep 5m
done

