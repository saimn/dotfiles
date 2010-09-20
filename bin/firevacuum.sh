#!/bin/sh

pgrep -x firefox -U $(id -u) > /dev/null && echo “Arrêtez Firefox !” && exit 1
for i in `find ~/.mozilla -name \*.sqlite`; do sqlite3 $i vacuum; done
for i in `find ~/.mozilla -name \*.sqlite`; do sqlite3 $i reindex; done
