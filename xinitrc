#!/bin/sh
# -*- coding: utf-8 -*-
#
# ~/.xinitrc
# Executed by startx (run your window manager from here)
#

# xset s off                     # screen-saver control
xset -b                          # no bell
xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/TTF
xset fp rehash
# xsetroot -cursor_name left_ptr # set the cursor

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Load local modmap
test -r $HOME/.Xmodmap && xmodmap $HOME/.Xmodmap

# Load .Xdefaults
test -r $HOME/.Xresources && xrdb -merge $HOME/.Xresources
test -r $HOME/.Xdefaults && xrdb -merge $HOME/.Xdefaults

# [ -f /etc/xprofile ] && . /etc/xprofile
# [ -f ~/.xprofile ] && . ~/.xprofile

#(scale,tile,seemless,center)
#setxkbmap -option ctrl:swapcaps &
#autocutsel -selection CLIPBOARD -fork &
#autocutsel -selection PRIMARY -fork &

# background
# xsetroot -solid "#303030"
# feh --bg-scale ~/Downloads/118-e.jpg &

# export OOO_FORCE_DESKTOP=gnome &
# xrandr --output default --mode 1440x900 --pos 0x0 &

# Fix Java GUIs
# wmname LG3D

# see https://bbs.archlinux.org/viewtopic.php?id=99175
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# KDE and Qt programs look
export QT_PLUGIN_PATH=$HOME/.kde4/lib/kde4/plugins/:/usr/lib/kde4/plugins/

# Autostart {{{

# emacs --daemon &

# Start agents
eval $(gpg-agent --daemon)
# eval `ssh-agent -s`
# eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)

# }}}

DEFAULT_SESSION=awesome

case $1 in
    xfce4)
        exec startxfce4
	;;
    awesome)
        xscreensaver -nosplash &
        exec awesome
	;;
    enlightenment)
        exec enlightenment_start
	;;
    kde)
        exec startkde
        ;;
    # gnome)
    #     exec gnome-session
    #     ;;
    *)
	exec $DEFAULT_SESSION
	;;
esac

# Terminate ssh-agent
# eval `ssh-agent -k`
