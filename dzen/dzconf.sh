#!/bin/sh
#
# common colors, fonts, etc.
#

# colors
colorBG="#303030"
colorFG="#606060"
colorFG2="#909090"
colorFG3="#aecf96"
colorFG4="#cc896d" # limey
colorFG5="#c4df90" # peachy
colorFG6="#ffffba" # yellowy
colorBG2="gray40"

dzen_iconpath=$HOME/.dzen/bitmaps

barFont="-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*"
barXFont="inconsolata:size=10:bold"
xftFont="xft: inconsolata-10"

[ "$HOST" = "fireball" ] && gdbarcmd=dzen2-gdbar || gdbarcmd=gdbar
[ "$HOST" = "fireball" ] && gcpubarcmd=dzen2-gcpubar || gcpubarcmd=gcpubar

#echo $gdbarcmd
#echo $gcpubarcmd
