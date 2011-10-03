#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""
Run unison and notify result
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Thanks to Alexis Métaireau:
http://blog.notmyidea.org/working-directly-on-your-server-how-to-backup-and-sync-your-dev-environment-with-unison.html

- To run this script with a cron job every 30 minutes:
*/30 * * * * source /path/to/.ssh/agent; env DISPLAY=:0 python2 /path/to/unison.py [/path/to/unison_cmd]
"""

import os
import sys
from datetime import datetime


DEFAULT_LOGFILE = "~/unison-sync.log"
PROGRAM_NAME = "Unison syncer"
ICON_PATH = '/usr/share/icons/Faenza/status/48/'


def sync(logfile=DEFAULT_LOGFILE, program_name=PROGRAM_NAME, UNISON_CMD='unison'):
    # call unison to make the sync & get the duration of the operation
    before = datetime.now()
    os.system('{0} -batch &> {1}'.format(UNISON_CMD, logfile))
    td = datetime.now() - before
    delta = (td.microseconds + (td.seconds + td.days * 24 * 3600) * 10**6) / 10**6

    # check what was the last entry in the log
    error = False
    display_message = True
    log = open(os.path.expanduser(logfile))
    lines = log.readlines()

    try:
        import pynotify
    except RuntimeError:
        # don't display the notification
        display_message = False

    if lines[-1].startswith('Nothing to do'):
        display_message = False
    else:
        files = ['-' + l.split('[END]')[1] for l in lines if "[END]" in l]
        message = ''.join(files)

        if lines[-1].startswith("Synchronization complete"):
            message += lines[-1]
            message += " It took {0}s.".format(delta)
        else:
            error = True

    if display_message:
        pynotify.init("Some Application or Title")
        if error:
            icon = ICON_PATH+'dialog-error.png'
        else:
            icon = ICON_PATH+'dialog-info.png'

        n = pynotify.Notification(program_name, message, icon)
        #n.set_urgency(pynotify.URGENCY_NORMAL)
        #n.set_timeout(pynotify.EXPIRES_NEVER)
        n.show()


if __name__ == "__main__":
    if len(sys.argv) == 2:
        unison_cmd = sys.argv[1]
        sync(UNISON_CMD=unison_cmd)
    else:
        sync()

