#!/bin/sh

if [[ $(hostname) == "goudes" ]]; then
    # gnome-settings-daemon &
    gnome-power-manager &
    # nm-applet --sm-disable &
    wicd-client &
    # alunn &
    dropboxd &
elif [[ $(hostname) == "fireball" ]]; then
    # /usr/libexec/gnome-settings-daemon &
    # gnome-volume-control-applet &
    # gpk-update-icon &
    numlockx &
    dropbox start &
fi

xbindkeys &
urxvtd -q -f -o &
# emacs --daemon &
# gnome-screensaver &
