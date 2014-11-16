#!/bin/bash
# -*- coding: utf-8 -*-

#{{{ Dotfiles
git clone git@github.com:saimn/dotfiles.git ~/lib/dotfiles

for i in "bashrc" "ctags" "gitconfig" "hgrc" "lessfilter" "mailcap" \
    "pythonrc" "pdbrc" "screenrc" "tmux.conf" "vim" "vimrc" "xbindkeysrc" \
    "Xresources" "Xmodmap" "xprofile"
do
    [ -e $HOME/.$i ] && rm $HOME/.$i
    ln -s $HOME/lib/dotfiles/$i $HOME/.$i
done

mkdir $HOME/.config/ranger
mkdir -p $HOME/lib/dotfiles/vim/tmp/{backup,swap,undo}

rm $HOME/.config/{awesome,zathura} $HOME/bin
ln -s $HOME/lib/dotfiles/bin/ $HOME/bin
ln -s $HOME/lib/dotfiles/awesome/ $HOME/.config/awesome
ln -s $HOME/lib/dotfiles/zathura/ $HOME/.config/zathura

ln -s $HOME/lib/dotfiles/compton.conf $HOME/.config/compton.conf

rm $HOME/.config/ranger/rc.conf
ln -s $HOME/lib/dotfiles/ranger/rc.conf $HOME/.config/ranger/rc.conf

#}}}

#{{{ ZSH
git clone https://github.com/sorin-ionescu/prezto.git ~/lib/prezto
rm ~/{.zprezto,.zpreztorc,.zshenv,.zshrc}
ln -s ~/lib/dotfiles/zshrc ~/.zshrc
ln -s ~/lib/dotfiles/zshenv ~/.zshenv
ln -s ~/lib/dotfiles/zpreztorc ~/.zpreztorc
ln -s ~/lib/prezto ~/.zprezto
#}}}

# {{{ Emacs
git clone git@github.com:saimn/dotemacs.git ~/lib/dotemacs/
rm ~/.emacs.d
ln -s ~/lib/dotemacs ~/.emacs.d
mkdir -p $HOME/lib/dotfiles/emacs.d/backup-files/
# }}}

# {{{ Mail
cd
rm .mutt .goobookrc .mairixrc .msmtprc .offlineimaprc .fdm.conf
ln -s ~/lib/mail/offlineimaprc .offlineimaprc
ln -s ~/lib/mail/goobookrc .goobookrc
ln -s ~/lib/mail/mairixrc .mairixrc
ln -s ~/lib/mail/msmtprc .msmtprc
ln -s ~/lib/mail/fdm.conf .fdm.conf
ln -s ~/lib/mail/mutt .mutt
# }}}

# {{{ Python
#sudo pip install virtualenv
#pip install --install-option="--user" virtualenvwrapper

#hg clone http://bitbucket.org/dhellmann/virtualenvwrapper ~/lib/python/virtualenvwrapper
#cd ~/lib/python/virtualenvwrapper
#sudo python setup.py install

mkdir -p $HOME/lib/{python,virtualenvs}
ln -s ~/lib/virtualenvs/  ~/.virtualenvs

rm $HOME/.config/ipython/profile_default/ipython_config.py
ln -s $HOME/lib/dotfiles/ipython/ipython_config.py \
    $HOME/.config/ipython/profile_default/ipython_config.py

mkdir -p $HOME/.config/matplotlib
rm -r $HOME/.matplotlib
rm -r $HOME/.config/matplotlib
ln -s $HOME/lib/dotfiles/matplotlibrc $HOME/.config/matplotlib/matplotlibrc
# }}}

#{{{ Other repos
# git clone git@github.com:saimn/color-theme-wombat.git           ~/lib/color-theme-wombat
git clone https://github.com/mitsuhiko/flask-sphinx-themes.git  ~/lib/flask-sphinx-themes
git clone https://github.com/jsmits/github-cli.git              ~/lib/github-cli
# git clone git://gitorious.org/mutt-notmuch/mutt-notmuch.git     ~/lib/mutt-notmuch
# git clone https://github.com/apenwarr/sshuttle.git              ~/lib/sshuttle
git clone https://github.com/gfxmonk/supergenpass.git           ~/lib/supergenpass

hg clone https://bitbucket.org/thayerwilliams/vimbrant          ~/lib/vimbrant
hg clone http://bitbucket.org/sjl/hg-prompt/                    ~/lib/hg-prompt
hg clone https://bitbucket.org/sjl/mercurial-cli-templates      ~/lib/mercurial-cli-templates
# }}}
