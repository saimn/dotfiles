# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

#
# Executes commands at the start of an interactive session.
#

# modify fpath before call compinit
fpath=(
    # "/usr/share/zsh/$ZSH_VERSION/functions"
    # "/usr/share/zsh/site-functions"
    "$HOME/lib/dotfiles/zsh"
    "$HOME/lib/dotfiles/zsh/completion"
    $fpath
)

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# source ~/lib/dotfiles/purepower
# source ~/lib/dotfiles/zsh/completion.zsh
source ~/lib/dotfiles/zsh/functions.zsh
source ~/lib/dotfiles/zsh/keys.zsh
source ~/lib/dotfiles/zsh/ssh.zsh
source ~/lib/dotfiles/zsh/aliases.zsh

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

__pyenv_version_ps1 ()
{
    local ret=$?;
    if [ -n "${PYENV_VERSION}" ]; then
        echo -n "(${PYENV_VERSION}) "
    fi
    return $?
}

# export PS1='\n$(pyenv version-name)\n'$PS1
if command -v pyenv 1>/dev/null 2>&1; then
    PS1="\$(__pyenv_version_ps1)${PS1}"
fi

if [ -n "$TMUX" ]; then
  function refresh {
    eval $(tmux show-env -s | grep '^SSH_')
    eval $(tmux show-env -s | grep '^DISPLAY')
  }
else
  function refresh { :; }
fi

# Watch new users
# watch=(all)
# LOGCHECK=5

[ -f $HOME/lib/dotfiles/zsh/locals.zsh ] && source $HOME/lib/dotfiles/zsh/locals.zsh

# pyenv
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

if command -v uv 1>/dev/null 2>&1; then
    eval "$(uv generate-shell-completion zsh)"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
