# -*- coding: utf-8 -*-

import sys

try:
    sys.setdefaultencoding('utf8')
except AttributeError:
    pass

# Automatically start the debugger on an exception
# http://code.activestate.com/recipes/65287/
# code snippet to be included in 'sitecustomize.py'


def info(type, value, tb):
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        # we are in interactive mode or we don't have a tty-like
        # device, so we call the default hook
        sys.__excepthook__(type, value, tb)
    else:
        import traceback, pdb
        # we are NOT in interactive mode, print the exception...
        traceback.print_exception(type, value, tb)
        print
        # ...then start the debugger in post-mortem mode.
        pdb.pm()

sys.excepthook = info
