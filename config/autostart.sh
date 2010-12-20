#!/bin/sh

# gnome keyring
#eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg)

#feh --bg-scale ~/Downloads/118-e.jpg &
#feh --bg-tile ~/.xmonad/background.png &

#export OOO_FORCE_DESKTOP=gnome &
#xrandr --output default --mode 1440x900 --pos 0x0 &

#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000 &

# stalonetray &
# exec xcompmgr &

if [[ $(hostname) == "goudes" ]]; then
    gnome-settings-daemon &
    gnome-power-manager &
    # nm-applet --sm-disable &
    wicd-client &
    alunn &
    dropboxd &
    gnome-agenda &
elif [[ $(hostname) == "fireball" ]]; then
    # /usr/libexec/gnome-settings-daemon &
    gnome-volume-control-applet &
    gpk-update-icon &
    numlockx &
    dropbox start &
fi

#nitrogen --restore &
# gnome-screensaver &
urxvtd -q -f -o &
# emacs --daemon &
# xbindkeys &
