#!/bin/sh

# feh --bg-scale ~/Downloads/118-e.jpg &
# feh --bg-tile ~/.xmonad/background.png &
# nitrogen --restore &

# export OOO_FORCE_DESKTOP=gnome &
# xrandr --output default --mode 1440x900 --pos 0x0 &
# trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000 &
# eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)

if [[ $(hostname) == "goudes" ]]; then
    eval $(gpg-agent --daemon)
    # gnome-settings-daemon &
    gnome-power-manager &
    # nm-applet --sm-disable &
    wicd-client &
    alunn &
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
