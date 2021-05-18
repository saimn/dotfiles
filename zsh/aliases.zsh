#!/usr/bin/env zsh

# per extension
alias -s conf="vim"
alias -s fits="ds9"
alias -s html="firefox -new-tab"
alias -s jpg="eog"
alias -s log="less"
alias -s pdf="evince"
alias -s png="eog"
alias -s ps="evince"

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi
