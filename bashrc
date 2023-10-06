# .bashrc

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
[ -f /etc/bashrc ] && source /etc/bashrc
[ -f /etc/bash.bashrc ] && source /etc/bash.bashrc

# better completion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
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

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

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
# PS1="\[\e[01;32m\]\u@\h\[\e[00m\]:\[\e[01;34m\][\w]\[\e[00m\]\$ "
PS1="\[\e[01;32m\]\h\[\e[00m\]:\[\e[01;34m\]\w\[\e[00m\]\$ "

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# Correction automatique des petites typos
shopt -s cdspell

shopt -s globstar
shopt -s autocd

#-------------------------------------
# PATH & ENV
#-------------------------------------

EDITOR=vim
#VISUAL="gvim -f"
#ALTERNATE_EDITOR=emacs
PAGER=less
PATH=$HOME/bin:$PATH
# BROWSER='firefox -new-tab'

export EDITOR VISUAL PAGER PATH ALTERNATE_EDITOR

# turn off .lesshst file
export LESSHISTFILE="-"

#-------------------------------------
# Alias definitions.
#-------------------------------------

alias ..='cd ..'
alias ...='cd ../..'

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

#-------------------------------------
# Useful commands
#-------------------------------------

if [ -n "$TMUX" ]; then
  function refresh {
    export $(tmux show-environment | grep "^SSH_AUTH_SOCK")
    export $(tmux show-environment | grep "^DISPLAY")
  }
else
  function refresh { :; }
fi

# run multiple Firefox
#export MOZ_NO_REMOTE=1

# calc
calc(){ awk "BEGIN{ print $* }" ;}

# Set host-specific config
[[ -f ~/.bashrc.$HOSTNAME ]] && . ~/.bashrc.$HOSTNAME
[[ -f ~/.bashrc.local ]] && . ~/.bashrc.local

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
