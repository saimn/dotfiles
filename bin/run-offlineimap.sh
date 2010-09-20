#!/bin/sh
# -*- coding: UTF8 -*-

PID=`pgrep offlineimap`

[ -n "$PID" ] && exit 1

offlineimap -o -u Noninteractive.Quiet &>/dev/null

exit 0
