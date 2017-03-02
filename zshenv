#
# Defines environment variables.
#

# {{{ Environment vars

#export LC_ALL="en_US.UTF-8"
#export LANG="en_US.UTF-8"

#export TERM='rxvt-256color'
# for tmux: export 256color
[ -n "$TMUX" ] && export TERM=screen-256color

# FZF conf
export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# [-n "$TMUX" ] && export FZF_TMUX=1

# default permissions
# umask 022

# Proxy HTTP / FTP
#export http_proxy="http://login:password@proxy.exemple.org:8080"
#export ftp_proxy="ftp://login:password@proxy.exemple.org:8080"

# Ne pas passer par le proxy pour les domaines locaux
#export no_proxy="exemple.org"

export BROWSER="firefox"
# run multiple Firefox
#export MOZ_NO_REMOTE=1

# for pcmanfm's list of apps
#export XDG_MENU_PREFIX=lxde-

# }}}

# {{{ Python
# export PYTHONPATH="$HOME/lib/python:$PYTHONPATH"
# export PYTHONSTARTUP="$HOME/.pythonrc"
# }}}

#
# Editors
#

#export VISUAL="emacsclient -c"
#export EDITOR="emacsclient -t"
#export ALTERNATE_EDITOR="emacs"

export VISUAL="gvim -f"
export EDITOR="vim"
export PAGER=less

#
# Language
#

if [[ -z "$LANG" ]]; then
  eval "$(locale)"
fi

#
# Paths
#

typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )


# Set the list of directories that Zsh searches for programs.
path=(
  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
  $path
)

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# export LESS_TERMCAP_mb=$'\E[01;31m'    # begin blink !
# export LESS_TERMCAP_md=$'\E[01;31m'    # begin bold
# export LESS_TERMCAP_me=$'\E[0m'        # end
# export LESS_TERMCAP_so=$'\E[01;44;33m' # début statusbar
# export LESS_TERMCAP_se=$'\E[0m'        # end
# export LESS_TERMCAP_us=$'\E[01;32m'    # begin underline
# export LESS_TERMCAP_ue=$'\E[0m'        # end
# export LESSHISTFILE="-"                # turn off .lesshst file

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

#
# Temporary Files
#

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
  if [[ ! -d "$TMPPREFIX" ]]; then
    mkdir -p "$TMPPREFIX"
  fi
