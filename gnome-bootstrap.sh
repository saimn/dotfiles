#!/bin/sh
#
# Bootstrap script to configure a fresh gnome-shell install
#

/usr/bin/gsettings set org.gnome.desktop.interface clock-show-date true

# evince
/usr/bin/gsettings set org.gnome.Evince.Default continuous false
/usr/bin/gsettings set org.gnome.Evince.Default show-sidebar false

# dock
# /usr/bin/gsettings set org.gnome.shell.extensions.dock position 'left'

# default apps
# /usr/bin/gsettings set org.gnome.desktop.default-applications.terminal exec urxvtc
# /usr/bin/gsettings set org.gnome.desktop.default-applications.terminal exec-arg "'-e'"

# keybindings
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings maximize "['<Super>m']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings panel-run-dialog "['<Super>F2']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>1']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>2']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>3']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>4']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down "['<Super>Down']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up "['<Super>Up']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings toggle-fullscreen "['<Super>f']"
/usr/bin/gsettings set org.gnome.desktop.wm.keybindings toggle-maximized "['<Super>space']"
/usr/bin/gsettings set org.gnome.mutter.keybindings toggle-tiled-left "['<Super>Left']"
/usr/bin/gsettings set org.gnome.mutter.keybindings toggle-tiled-right "['<Super>Right']"
/usr/bin/gsettings set org.gnome.shell.keybindings focus-active-notification "['<Super>u']"
/usr/bin/gsettings set org.gnome.shell.keybindings toggle-application-view "['<Super>a']"
/usr/bin/gsettings set org.gnome.shell.keybindings toggle-message-tray "['<Super>n']"
/usr/bin/gsettings set org.gnome.shell.keybindings toggle-overview "['<Super>s']"



# meteo
# /usr/bin/gsettings set org.gnome.shell.extensions.weather city Marseille
# /usr/bin/gsettings set org.gnome.shell.extensions.weather unit celsius
# /usr/bin/gsettings set org.gnome.shell.extensions.weather woeid 610264
# /usr/bin/gsettings set org.gnome.shell.extensions.weather use-symbolic-icons true

# themes
# cd ~/.themes
# wget deviantart.com/download/203936861/zukitwo_by_lassekongo83-d3df2ot.zip
# unzip zukitwo*.zip && rm -f zukitwo*.zip

# conky
# wget -O ~/.conkyrc www.tux-planet.fr/public/conf/conky/conky-gotham
