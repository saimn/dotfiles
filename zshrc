#!/usr/bin/env zsh
# Global zsh configuration

# {{{ Startup

uname -sr
uptime

# }}}


# {{{ History

setopt EXTENDED_HISTORY      # add a timestamp and the duration of each command
setopt SHARE_HISTORY         # _all_ zsh sessions share the same history files
setopt HIST_IGNORE_ALL_DUPS  # ignores duplications

HISTFILE=~/.zsh/history
HISTSIZE=1000000  # nombre de lignes à conserver dans l'historique
SAVEHIST=1000000  # nombre de lignes enregistrées après que vous quittez le shell

export HISTFILE HISTSIZE SAVEHIST

# }}}

# {{{ Global config

autoload -U colors
colors

export LISTPROMPT       # in order to scroll if completion list is too big
export DIRSTACKSIZE=20  # Number of directory to keep in the stack

unsetopt beep
unsetopt notify

setopt auto_cd          # % /usr/local is equivalent to cd /usr/local
setopt nohup            # don't send HUP signal when closing term session
setopt extended_glob    # in order to use #, ~ and ^ for filename generation
setopt always_to_end    # move to cursor to the end after completion
setopt notify           # report the status of backgrounds jobs immediately
setopt correct          # try to correct the spelling if possible
#setopt rmstarwait       # wait 10 seconds before querying for a rm which contains a *
setopt printexitvalue   # show the exit-value if > 0
setopt auto_pushd       # Always push directory in stack
setopt pushdminus       # Invert the behavior of cd -<tab> cd +<tab>
setopt histverify       # Verify history expansion
setopt prompt_subst     # permit parameter expansion, etc., in prompt

#setopt auto_list auto_menu  # Complétion
#setopt no_hup no_check_jobs # détacher les procesus en arriere plan lors d'un exit

# Don't erase file with >, use >| (overwrite) or >> (append) instead
set -C

# Watch new users
watch=(all)
LOGCHECK=5

# Quand on fait ^W : s'arrêter aux /
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# }}}

source $HOME/.zsh/aliases.zsh
source $HOME/.zsh/completion.zsh
source $HOME/.zsh/keys.zsh

# {{{ Functions

source $HOME/.zsh/rc/functions.rc
source $HOME/.zsh/rc/ssh.rc
source $HOME/.zsh/rc/prompt.rc

# }}}

# {{{ Per host config

local os host

# per host resource file
host=${$(hostname)//.*/}
[ -f "$HOME/.zsh/${host}.zsh" ] && source "$HOME/.zsh/${host}.zsh"

# per OS resource file
# os=$(uname)
# [ -f "$HOME/.zsh/rc.os/${os}.zsh" ] && source "$HOME/.zsh/rc.os/${os}.zsh"

# Global resource files
#for file in $HOME/.zsh/rc/*.rc; do
  #source $file
#done

# Distrib specific
[ -r /etc/debian_version ] && source $HOME/.zsh/debian.zsh

# Local file
[[ -f ~/.zsh/zshrc.local ]] && source ~/.zsh/zshrc.local

# }}}

# vim:filetype=zsh:tabstop=4:shiftwidth=2:
#fdm=marker:
