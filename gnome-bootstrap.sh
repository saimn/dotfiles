#!/bin/sh
#
# Bootstrap script to configure a fresh gnome-shell install
#

gsettings set org.gnome.desktop.interface clock-show-date true

# evince
gsettings set org.gnome.Evince.Default continuous false
gsettings set org.gnome.Evince.Default show-sidebar false

# dock
# gsettings set org.gnome.shell.extensions.dock position 'left'

# default apps
gsettings set org.gnome.desktop.default-applications.terminal exec urxvtc
gsettings set org.gnome.desktop.default-applications.terminal exec-arg "'-e'"

# keybindings
gsettings set org.gnome.desktop.wm.keybindings maximize "['<Super>m']"
gsettings set org.gnome.desktop.wm.keybindings panel-run-dialog "['<Super>F2']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>1']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>2']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>3']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>4']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down "['<Super>Down']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up "['<Super>Up']"
gsettings set org.gnome.desktop.wm.keybindings toggle-fullscreen "['<Super>f']"
gsettings set org.gnome.desktop.wm.keybindings toggle-maximized "['<Super>space']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-left "['<Super>Left']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-right "['<Super>Right']"
gsettings set org.gnome.shell.keybindings focus-active-notification "['<Super>u']"
gsettings set org.gnome.shell.keybindings toggle-application-view "['<Super>a']"
gsettings set org.gnome.shell.keybindings toggle-message-tray "['<Super>n']"
gsettings set org.gnome.shell.keybindings toggle-overview "['<Super>s']"



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
