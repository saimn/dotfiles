#!/usr/bin/env zsh

# {{{ Global config

# autoload -U colors
# colors

export LISTPROMPT       # in order to scroll if completion list is too big
export DIRSTACKSIZE=20  # Number of directory to keep in the stack

unsetopt beep
unsetopt notify

# setopt correct              # try to correct the spelling if possible
# setopt rmstarwait           # wait 10 seconds before querying for a rm which contains a *

# setopt auto_pushd           # make cd push the old directory onto the directory stack.
# setopt pushdminus           # Invert the behavior of cd -<tab> cd +<tab>
# setopt pushd_ignore_dups    # don't push the same dir twice.

setopt printexitvalue       # show the exit-value if > 0
setopt prompt_subst         # permit parameter expansion, etc., in prompt

setopt auto_cd              # if a command is issued that can't be executed as a normal command,
                            # and the command is the name of a directory, perform the cd command
setopt extended_glob        # in order to use #, ~ and ^ for filename generation
                            # grep word *~(*.gz|*.bz|*.bz2|*.zip|*.Z) ->
                            # -> searches for word not in compressed files
                            # don't forget to quote '^', '~' and '#'!
setopt longlistjobs         # display PID when suspending processes as well
setopt notify               # report the status of backgrounds jobs immediately
setopt hash_list_all        # Whenever a command completion is attempted, make sure \
                            # the entire command path is hashed first.
setopt nohup                # and don't kill them, either
setopt nonomatch            # try to avoid the 'zsh: no matches found...'
setopt noglobdots           # * shouldn't match dotfiles. ever.
setopt noshwordsplit        # use zsh style word splitting
setopt unset                # don't error out when unset parameters are used

#setopt no_hup no_check_jobs # détacher les procesus en arriere plan lors d'un exit

# Don't erase file with >, use >| (overwrite) or >> (append) instead
# set -C

# Watch new users
watch=(all)
LOGCHECK=5

# }}}

