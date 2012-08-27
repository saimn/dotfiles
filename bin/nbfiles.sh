#!/bin/bash
# -*- coding: utf-8 -*-

# return the number of files in the given directories
# Usage : nbmail.sh /path/to/dir /another/dir

find "$@" -type f | wc -l | tr -d " "
