#!/bin/sh
# -*- coding: utf-8 -*-
#
# ~/.xinitrc
#

# Executed by startx (run your window manager from here)
#xset s off

# no bell
xset -b

# Make sure this is before the 'exec' command or it won't be executed.
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

# Start awesome
# awesome
exec ck-launch-session awesome

# Terminate ssh-agent
# eval `ssh-agent -k`
