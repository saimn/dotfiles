#!/bin/sh
# -*- coding: UTF8 -*-

# read the pid of any still-running instance
read -r pid < <(pgrep offlineimap)

# need be, kill it
[[ -n "$pid" ]] && kill -9 $pid

# (re)sync
offlineimap -o -u Noninteractive.Quiet &>/dev/null &
