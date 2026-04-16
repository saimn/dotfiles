#!/bin/bash

bin completion zsh > ~/.zsh-complete/_bin

bin install github.com/sharkdp/fd
fd --gen-completions zsh > ~/.zsh-complete/_fd

bin install github.com/BurntSushi/ripgrep
rg --generate complete-zsh > ~/.zsh-complete/_rg

bin install github.com/sxyazi/yazi

bin install github.com/neovim/neovim

bin install github.com/astral-sh/uv
