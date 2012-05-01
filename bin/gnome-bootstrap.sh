#!/bin/sh
#
# Bootstrap script to configure a fresh gnome-shell install
#

gsettings set org.gnome.shell.clock show-date true

# evince
gsettings set org.gnome.Evince.Default continuous false
gsettings set org.gnome.Evince.Default show-sidebar false

# dock
# gsettings set org.gnome.shell.extensions.dock position 'left'

# default apps
gsettings set org.gnome.desktop.default-applications.terminal exec urxvtc
gsettings set org.gnome.desktop.default-applications.terminal exec-arg "'-e'"

# meteo
# gsettings set org.gnome.shell.extensions.weather city Marseille
# gsettings set org.gnome.shell.extensions.weather unit celsius
# gsettings set org.gnome.shell.extensions.weather woeid 610264
# gsettings set org.gnome.shell.extensions.weather use-symbolic-icons true

# themes
# cd ~/.themes
# wget deviantart.com/download/203936861/zukitwo_by_lassekongo83-d3df2ot.zip
# unzip zukitwo*.zip && rm -f zukitwo*.zip

# conky
# wget -O ~/.conkyrc www.tux-planet.fr/public/conf/conky/conky-gotham
