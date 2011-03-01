#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Copyright (c) 2008, 2009 Sebastian Wiesner <lunaryorn@googlemail.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.


import re
import os

from Pymacs import lisp


def qualified_module_name(buffer=None):
    """
    Determine the qualified name of the python module which is currently
    edited in the given ``buffer``.  If unset, ``buffer`` defaults to the
    current buffer.

    If the buffer has no associated filename, this function returns None.
    """
    if not buffer:
        buffer = lisp.current_buffer()
    if isinstance(buffer, basestring):
        buffer = lisp.get_buffer(buffer)
        if buffer is None:
            lisp.error('No such buffer: %s', buffername)
    filename = lisp.buffer_file_name(buffer)
    if filename is None:
        return None

    filepath = os.path.normpath(os.path.abspath(filename))
    modules = []
    modname = os.path.basename(os.path.splitext(filepath)[0])
    if modname != '__init__':
        modules.append(modname)

    directory = os.path.dirname(filepath)
    parent = os.path.dirname(directory)
    while directory != parent:
        if not is_package(directory):
            break
        pkgname = os.path.basename(directory)
        modules.append(pkgname)
        directory = parent
        parent = os.path.dirname(directory)

    modules.reverse()
    return '.'.join(modules)
qualified_module_name.interaction = 'bbuffer: '


def is_package(directory):
    base = os.path.join(directory, '__init__.py')
    for candidate in ('', 'c', 'o'):
        if os.path.isfile(base + candidate):
            return True
    return False
