#!/bin/bash
# -*- coding: utf-8 -*-

DIR=$HOME/dotfiles/

mkdir -p ~/dotfiles_old

cd $DIR
for i in *
do
   mv ~/.$i ~/dotfiles_old/
   ln -s $DIR/$i ~/.$i
done

#rm ~/.make.sh
