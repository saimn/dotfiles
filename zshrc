#!/usr/bin/env zsh
# Global zsh configuration

# {{{ Startup

uname -sr
uptime

# }}}

# {{{ Environment vars

export PATH="$HOME/bin:$HOME/.zsh/bin:$HOME/.cabal/bin:/usr/local/bin:$PATH"
export PYTHONPATH="$HOME/lib/python:$PYTHONPATH"
export PYTHONSTARTUP="$HOME/.pythonrc"

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

# {{{ keys

# Use emacs bindkey
bindkey -e

#bindkey '^[[A'    up-line-or-history
#bindkey '^[[B'    down-line-or-history
#bindkey '^U'      backward-kill-line
#bindkey '^[[3~'   delete-char             # suppr
#bindkey '^[[7~'   beginning-of-line
#bindkey '^[[8~'   end-of-line

bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "^H" backward-delete-word

# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
bindkey '\eOc' forward-word
bindkey '\eOd' backward-word

#zle -N insert-root-prefix
autoload -U insert-root-prefix
zle -N insert-root-prefix
bindkey "^[f" insert-root-prefix

# Use control-<left|right> arrows to move through words
bindkey '^[[1;5D' vi-backward-word
bindkey '^[[1;5C' vi-forward-word

# }}}

# {{{ Aliases

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ] || [ -x /bin/dircolors ]; then
  if [[ -r ~/.dir_colors ]]; then
    eval `dircolors -b ~/.dir_colors`
  else
    eval `dircolors -b`
  fi

  alias ls='ls -F --color=auto'
  alias dir='dir --color=auto'
  #alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  #alias fgrep='fgrep --color=auto'
  #alias egrep='egrep --color=auto'
fi

# Quelques options de ls :
# -F: Avoir des / après les noms de dossiers > plus de couleur
# -G: cacher les noms de groupe

alias ll='ls -l --group-directories-first'  # listage de répertoire détaillé
alias lh='ll -Gh'                           # idem avec tailles human
alias lt='ll -tr'                           # tri par date de modif
alias la='ls -la'
alias u="cd .. && ls"
alias lsd='ls -ld *(-/DN)'

# -p : conserve les dates, droits lors de la copie
alias cp='cp -p'
#alias mv='mv -v'
#alias rm='rm -v'

alias du='du -h --max-depth=1'
alias dusort='du -x --block-size=1048576 | sort -nr'
alias df='df -h'

alias :e="\$EDITOR"
alias :wq='exit'
alias :q!='exit'
alias bashfr="lynx --dump --display_charset=utf8 \"http://danstonchat.com/random.html\" | awk '\$1~\"#\" && \$0!~\"RSS\" { getline; while (\$1!~\"#\") { print \$0; getline;}; exit}'"
alias top-10="sed -e 's/sudo //' $HOME/.zsh/history |  cut -d' ' -f1 | sort | uniq -c | sort -rg | head"

# per extentions
alias -s pdf="zathura"
alias -s ps="evince"
alias -s png="mirage"
alias -s jpg="mirage"
alias -s log="tail -f"
alias -s conf="vim"

# misc commands
alias ht='urxvt -name htop -title htop +sb -geometry 80x6 -e htop &'
alias firedev='firefox -no-remote -P Webdev'
alias svnd='svn diff --diff-cmd "~/bin/svnvimdiff"'
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'

alias e="emacsclient"
alias ec="emacsclient -n -c"
alias et="emacsclient -t"
alias v='exec urxvtc -e "bash" -c "exec vim $@" >> /dev/null &'
alias vi="vim"
alias sv='exec urxvtc -e "bash" -c "exec sudo vim $@" >> /dev/null &'

# simple webserver on port 8000
alias webshare='python -m SimpleHTTPServer'

# open & close dropbox coffre
#alias sync="~/.dropbox-dist/dropboxd &"
alias coffre-open='encfs $HOME/Dropbox/coffre $HOME/Documents/coffre-dropbox'
alias coffre-close='fusermount -u $HOME/Documents/coffre-dropbox'

# ping (since control-c don't work for break ping)
#alias ping="ping -c 3"

# start mutt with list mailboxes
#[[ -x `which mutt` ]] && alias mutt="mutt -y"

# }}}

# {{{ Completion

# My functions (don't forget to modify fpath before call compinit !!)
fpath=($HOME/.zsh/functions $fpath)

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
#zstyle ':completion:*' list-colors ${(s.:.)LSCOLORS}

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

# ssh hosts
local _myhosts
_myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

# }}}

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
