#!/bin/sh
#
#   -rwx- /etc/laptop-mode/batt-stop/vicious.sh
#
# Script for laptop-mode start-stop-programs module

vicious_start() {
    /bin/su - user -c "/bin/echo 'vicious.activate()' | /usr/bin/awesome-client"
    /bin/echo "Activated Vicious widgets for Awesome window manager."
}

vicious_stop() {
    /bin/su - user -c "/bin/echo 'vicious.suspend()' | /usr/bin/awesome-client"
    # Some widgets are important while running on battery, BAT widget in particular
    /bin/su - user -c "/bin/echo 'vicious.activate(batwidget)' | /usr/bin/awesome-client"
    /bin/echo "Suspended Vicious widgets for Awesome window manager."
}

case "$1" in
  'start')
      vicious_start
      ;;
  'stop')
      vicious_stop
      ;;
*)
      echo "usage $0 start|stop"
esac
