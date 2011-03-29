#!/usr/bin/env zsh
# Global zsh configuration

# {{{ Environment vars

export PATH="$HOME/bin:$HOME/.zsh/bin:$HOME/.cabal/bin:/usr/local/bin:$PATH"
export PYTHONPATH="$HOME/lib/python:$PYTHONPATH"
export PYTHONSTARTUP="$HOME/.pythonrc"

#export LC_ALL="en_US.UTF-8"
#export LANG="en_US.UTF-8"

#export TERM='rxvt-256color'
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -c"
#export EDITOR="gvim -f"
export ALTERNATE_EDITOR="emacs"

# if [ -x `which most` ]; then
#    export PAGER=most
# elif [ -x `which less` ]; then

export PAGER=less
export LESS="-ir"
export LESS_TERMCAP_mb=$'\E[01;31m'    # begin blink !
export LESS_TERMCAP_md=$'\E[01;31m'    # begin bold
export LESS_TERMCAP_me=$'\E[0m'        # end
export LESS_TERMCAP_so=$'\E[01;44;33m' # début statusbar
export LESS_TERMCAP_se=$'\E[0m'        # end
export LESS_TERMCAP_us=$'\E[01;32m'    # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # end
export LESSHISTFILE="-"                # turn off .lesshst file

if [ "$HOSTNAME" = "goudes" ]; then
    export LESSOPEN="| /usr/bin/lesspipe %s"
    export LESSCLOSE="/usr/bin/lesspipe %s %s"
elif [ "$HOSTNAME" = "fireball" ]; then
    export LESSOPEN="| /usr/bin/lesspipe.sh %s"
    export LESSCLOSE="/usr/bin/lesspipe.sh %s %s"
fi

# Permissions rw-r--r-- pour les fichiers crées
# et rwxr-xr-x pour les répertoires crées
umask 022

# Proxy HTTP / FTP sans mot de passe
#export http_proxy="http://proxy.exemple.org:8080"
#export ftp_proxy="ftp://proxy.exemple.org:8080"

# Proxy HTTP / FTP avec mot de passe
#export http_proxy="http://login:password@proxy.exemple.org:8080"
#export ftp_proxy="ftp://login:password@proxy.exemple.org:8080"

# Ne pas passer par le proxy pour les domaines locaux
#export no_proxy="exemple.org"

# De la couleur pour grep
export GREP_OPTIONS='--color=auto'

export BROWSER="firefox"
# run multiple Firefox
#export MOZ_NO_REMOTE=1

# repertoire texlive local
[ "$HOSTNAME" = "goudes" ] && MYTEXMF="miktex-texmf" || MYTEXMF=".texmf"
export TEXMFHOME=$HOME/$MYTEXMF

# }}}
