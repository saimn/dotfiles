#!/usr/bin/env zsh

# {{{ Completion

# My functions (don't forget to modify fpath before call compinit !!)
# fpath=($HOME/.zsh/functions $fpath)

# setopt auto_list auto_menu  # Complétion
# setopt always_to_end        # move to cursor to the end after completion
# setopt completeinword       # not just at the end

# Quand on fait ^W : s'arrêter aux /
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# in order to have many completion function run comptinstall !
autoload -U zutil
autoload -U compinit
autoload -U complist
compinit

#zmodload zsh/complist
#zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
#zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
#/usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
#zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
#zstyle ':completion:*:rm:*' ignore-line yes
#zstyle ':completion:*:mv:*' ignore-line yes
#zstyle ':completion:*:cp:*' ignore-line yes

# Use LS_COLORS for color completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Global completion behavior
zstyle ':completion:*' completer _complete _prefix _approximate
zstyle ':completion:*:complete:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' use-ip true
zstyle ':completion:*' menu select

# Use 'ps -au$USER' for fetch user process list
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"

# Verbose mode
zstyle ':completion:*:descriptions' format '%B%d%b'
#zstyle ':completion:*:descriptions' format '%U%B%d%b%u'

# Use cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh

# prevent CVS files/directory completion
# if [[ -x $(which cvs) ]]; then
#   zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
#   zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'
# fi

zstyle ':completion:*' ignore-parents parent pwd

zstyle ':completion:*:*:zless:*' file-patterns '*(-/):directories *.gz:all-files'
zstyle ':completion:*:*:gqview:*' file-patterns '*(-/):directories :(#i)*.(png|jpeg|jpg):all-files'
zstyle ':completion:*:*:lintian:*' file-patterns '*(-/):directories *.deb'
zstyle ':completion:*:*:evince:*' file-patterns '*(-/):directories (#i)*.(pdf|ps)'

zstyle ':completion:*:*:less:*' ignored-patterns '*.gz'
zstyle ':completion:*:*:zcompile:*' ignored-patterns '(*~|*.zwc)'

# few simple completion definitions
compdef _hosts mtr
compdef _hosts rdesktop
compdef _gnu_generic sort

# Some zstyle specific to vi/vim
zstyle ':completion:*:*:vi*:*' file-sort modification
zstyle ':completion:*:*:vi*:*' ignored-patterns '*.(o|class)'

# Prevent aptitude-* to be complete, directly or via sudo
zstyle ':completion:*:complete:-command-::commands' ignored-patterns 'aptitude-*'
zstyle ':completion:*:*:sudo:*:commands' ignored-patterns 'aptitude-*'

xdvi() { command xdvi ${*:-*.dvi(om[1])} }
zstyle ':completion:*:*:xdvi:*' menu yes select
zstyle ':completion:*:*:xdvi:*' file-sort time

# ssh hosts
# local _myhosts
# _myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
# zstyle ':completion:*' hosts $_myhosts

# }}}
