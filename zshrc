#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# sourced by omz but does not work as done before the setting of WORKON_HOME
# source /usr/bin/virtualenvwrapper.sh

source ~/lib/dotfiles/zsh/aliases.zsh
# source ~/lib/dotfiles/zsh/completion.zsh
source ~/lib/dotfiles/zsh/functions.zsh
source ~/lib/dotfiles/zsh/keys.zsh
# source ~/lib/dotfiles/zsh/opts.zsh

source ~/lib/dotfiles/zsh/locals.zsh
