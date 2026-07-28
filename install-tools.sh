#!/bin/bash

#
# bin
#

# bin completion zsh > ~/.zsh-complete/_bin
# bin install github.com/astral-sh/uv
# bin install github.com/BurntSushi/ripgrep
# bin install github.com/neovim/neovim
# bin install github.com/sharkdp/fd
# bin install github.com/sxyazi/yazi

#
# pixi
#

# curl -fsSL https://pixi.sh/install.sh | sh

pixi global install \
    bat \
    bear \
    clang clang-tools \
    fd-find \
    fzf \
    nodejs \
    nvim pynvim \
    pre-commit \
    ripgrep \
    ruff \
    universal-ctags \
    uv \
    yazi

#
# completion
#

fd --gen-completions zsh > ~/.zsh-complete/_fd
pixi completion --shell zsh > ~/.zsh-complete/_pixi
rg --generate complete-zsh > ~/.zsh-complete/_rg
