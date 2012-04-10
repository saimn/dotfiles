#!/bin/bash
# -*- coding: utf-8 -*-

#{{{ Dotfiles
git clone git@github.com:saimn/dotfiles.git ~/lib/dotfiles

for i in "ackrc" "bashrc" "cliverc" "ctags" "dzen" "gitconfig" \
    "gmrunrc" "hgrc" "lessfilter" "mailcap" "pentadactyl" "pentadactylrc" \
    "pythonrc" "screenrc" "tmux.conf" "vim" "vimrc" "xbindkeysrc" \
    "Xdefaults" "Xmodmap" "xprofile" "xsession"
do
    rm $HOME/.$i
    ln -s $HOME/lib/dotfiles/$i $HOME/.$i
done

rm ~/.config/zathura ~/.config/awesome ~/bin
ln -s $HOME/lib/dotfiles/bin/ $HOME/bin
ln -s $HOME/lib/dotfiles/awesome/ $HOME/.config/awesome
ln -s $HOME/lib/dotfiles/zathura/ $HOME/.config/zathura

mkdir -p $HOME/lib/dotfiles/vim/tmp/{backup,swap,undo}

#}}}

#{{{ ZSH
git clone git@github.com:saimn/oh-my-zsh.git ~/lib/oh-my-zsh
rm ~/.zshenv ~/.zshrc
ln -s ~/lib/oh-my-zsh/zshrc ~/.zshrc
ln -s ~/lib/oh-my-zsh/zshenv ~/.zshenv
#}}}

# {{{ Emacs
git clone git@github.com:saimn/dotemacs.git ~/lib/dotemacs/
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
mkdir -p $HOME/lib/python
mkdir -p $HOME/lib/virtualenvs

rm $HOME/.config/ipython/profile_default/ipython_config.py
ln -s $HOME/lib/dotfiles/ipython/ipython_config.py \
    $HOME/.config/ipython/profile_default/ipython_config.py
# }}}

#{{{ Other repos
git clone git@github.com:saimn/color-theme-tangotango.git       ~/lib/color-theme-tangotango
git clone git@github.com:saimn/color-theme-wombat.git           ~/lib/color-theme-wombat
git clone https://github.com/jsmits/github-cli.git              ~/lib/github-cli/
git clone git://gitorious.org/mutt-notmuch/mutt-notmuch.git     ~/lib/mutt-notmuch/
git clone https://github.com/apenwarr/sshuttle.git              ~/lib/sshuttle/
git clone https://github.com/gfxmonk/supergenpass.git           ~/lib/supergenpass/
git clone https://github.com/mitsuhiko/flask-sphinx-themes.git  ~/lib/flask-sphinx-themes/
hg  clone https://bitbucket.org/thayerwilliams/vimbrant         ~/lib/vimbrant/
# }}}

#git clone https://github.com/pypa/pip.git ~/lib/python/pip
#cd ~/lib/python/pip
#sudo python setup.py install
#cd

#sudo pip install virtualenv
#pip install --install-option="--user" virtualenvwrapper

#hg clone http://bitbucket.org/dhellmann/virtualenvwrapper ~/lib/python/virtualenvwrapper
#cd ~/lib/python/virtualenvwrapper
#sudo python setup.py install
#cd

