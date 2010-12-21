#!/bin/sh

launch_xmonad(){
   gconftool-2 -t string -s /desktop/gnome/applications/window_manager/current xmonad
   gconftool-2 -t string -s /desktop/gnome/session/required_components/windowmanager xmonad
   killall metacity; xmonad &
}

launch_metacity(){
   gconftool-2 -t string -s /desktop/gnome/applications/window_manager/current metacity
   gconftool-2 -t string -s /desktop/gnome/session/required_components/windowmanager metacity
   killall xmonad; metacity --replace &
}

case $1 in
  "--xmonad")    launch_xmonad;     exit 0;;
  "--metacity")  launch_metacity;   exit 0;;
esac

