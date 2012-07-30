# .bashrc

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# better completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# completion for sudo
if [ "$PS1" ]; then
  complete -cf sudo
fi

# no longer necessary to hit the <Tab> key twice to produce completions
set show-all-if-ambiguous on

set completion-ignore-case On

#-------------------------------------
# HISTORY
#-------------------------------------

# ignoredups: don't put duplicate lines in the history
# ignorespace: ignore line starting with a space
export HISTCONTROL=ignoreboth
# lignes de l'historique par session bash
export HISTSIZE=50000
# lignes de l'historique conservées
export HISTFILESIZE=20000
# exclure des commandes de votre historique
#export HISTIGNORE="&:ls:cd:ll:la:ping:exit"
# append to the history file, don't overwrite it
shopt -s histappend

#-------------------------------------
# TERMINAL
#-------------------------------------

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# prompt
#PS1="[\u@\h: \w]$ "
PS1="\[\e[01;32m\]\u@\h\[\e[00m\]:\[\e[01;34m\][\w]\[\e[00m\]\$ "

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# Correction automatique des petites typos
shopt -s cdspell

shopt -s globstar
shopt -s autocd

#-------------------------------------
# PATH & ENV
#-------------------------------------

EDITOR=vim
VISUAL="gvim -f"
#ALTERNATE_EDITOR=emacs
PAGER=less
PATH=$PATH:$HOME/bin
BROWSER='firefox -new-tab'

export EDITOR VISUAL PAGER PATH ALTERNATE_EDITOR BROWSER

# repertoire texlive local
[ "$HOSTNAME" = "goudes" ] && MYTEXMF="miktex-texmf" || MYTEXMF=".texmf"
export TEXMFHOME=$HOME/$MYTEXMF

# turn off .lesshst file
export LESSHISTFILE="-"

#-------------------------------------
# Alias definitions.
#-------------------------------------

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ] || [ -x /bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# Quelques options de ls :
# -F: Avoir des / après les noms de dossiers > plus de couleur
# -G: cacher les noms de groupe

# Obtenir un listage de répertoire détaillé
alias l='ls -l --group-directories-first'
# idem avec tailles human
alias ll='l -Gh'
# tri par date de modif
alias lt='ll -tr'
# Lister avec les fichiers cachés
alias la='ls -la'
# Remonter d'un dossier et ls
alias u="cd .. && ls"

# -p : conserve les dates, droits lors de la copie
alias cp='cp -p'
#alias mv='mv -v'
#alias rm='rm -v'

alias ..='cd ..'
alias cd..='cd ..'
alias ...='cd ../..'

alias du='du -h --max-depth=1'
alias dusort='du -x --block-size=1048576 | sort -nr'
alias df='df -h'

alias ht='gnome-terminal -e htop -t htop --geometry=80x6 &'
alias uht='urxvt -name htop -title htop +sb -geometry 80x6 -e htop &'

alias v="vim"
alias sv="sudo vim"
#alias sync="~/.dropbox-dist/dropboxd &"

#-------------------------------------
# Useful commands
#-------------------------------------

alias svnd='svn diff --diff-cmd "~/bin/svnvimdiff"'

# simple webserver on port 8000
alias webshare='python -m SimpleHTTPServer'

# open & close dropbox coffre
alias coffre-open='encfs $HOME/Dropbox/coffre $HOME/Documents/coffre-dropbox'
alias coffre-close='fusermount -u $HOME/Documents/coffre-dropbox'

# run multiple Firefox
#export MOZ_NO_REMOTE=1

# calc
calc(){ awk "BEGIN{ print $* }" ;}



# Set host-specific config
[[ -f ~/.bashrc.$HOSTNAME ]] && . ~/.bashrc.$HOSTNAME

# Display stuff after login
# date
# have pom && pom
# echo
# have fortune && fortune -c
# echo

# vim: fdm=marker ts=4 sw=4 et:
