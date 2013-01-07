#!/bin/sh
# -*- coding: utf-8 -*-
#
# ~/.xinitrc
#

# Executed by startx (run your window manager from here)
#xset s off

# no bell
xset -b

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Make sure this is before the 'exec' command or it won't be executed.
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

xscreensaver -nosplash &
emacs --daemon &

# Start awesome
# exec dbus-launch awesome
# exec awesome

DEFAULT_SESSION=awesome

case $1 in
    xfce4)
	exec startxfce4
	;;
    awesome)
	exec awesome
	;;
    enlightenment)
	exec enlightenment_start
	;;
    # kde)
    #     exec startkde
    #     ;;
    # gnome)
    #     exec gnome-session
    #     ;;
    *)
	exec $DEFAULT_SESSION
	;;
esac

# Terminate ssh-agent
# eval `ssh-agent -k`
