#!/bin/bash

display_number=0
dbus_session_file=~/.dbus/session-bus/$(cat /var/lib/dbus/machine-id)-$display_number
suppress_warnings="no"

if [ -e "$dbus_session_file" ]; then
  . "$dbus_session_file"
  export DBUS_SESSION_BUS_ADDRESS DBUS_SESSION_BUS_PID

  if [ $suppress_warnings = "yes" ];then
    /usr/bin/offlineimap -o $* 2>&1 | grep --invert-match 'Warning'
  else
    /usr/bin/offlineimap -o $*
  fi
fi
