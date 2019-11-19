#
# Executes commands at the start of an interactive session.
#

# PATH="$HOME/bin:${PATH}"
# PATH="$HOME/bin/html2rst:${PATH}"
# PATH="$HOME/bin/html2text:${PATH}"
# PATH="$HOME/.local/bin:${PATH}"
# export PATH

# modify fpath before call compinit
fpath=( "$HOME/lib/dotfiles/zsh" "$HOME/lib/dotfiles/zsh/completion" $fpath )

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# Python
# export WORKON_HOME=$HOME/.virtualenvs
# export PROJECT_HOME=$HOME/Dev

# [[ -f /usr/bin/virtualenvwrapper.sh ]] && source /usr/bin/virtualenvwrapper.sh

source ~/lib/dotfiles/purepower
# source ~/lib/dotfiles/zsh/completion.zsh
source ~/lib/dotfiles/zsh/functions.zsh
source ~/lib/dotfiles/zsh/keys.zsh
source ~/lib/dotfiles/zsh/ssh.zsh
source ~/lib/dotfiles/zsh/aliases.zsh

# fzf via local installation
if [ -e ~/.fzf ]; then
    export PATH="$PATH:$HOME/.fzf/bin"
    source ~/.fzf/shell/key-bindings.zsh
    source ~/.fzf/shell/completion.zsh
fi
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Watch new users
watch=(all)
LOGCHECK=5

source ~/lib/dotfiles/zsh/locals.zsh

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
# export PYENV_VERSION=3.6.3
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# added by travis gem
[ -f /home/simon/.travis/travis.sh ] && source /home/simon/.travis/travis.sh
