#!/usr/bin/env zsh

# {{{ keys

# Use emacs bindkey
# bindkey -e

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

# zle -N zsh/zle/insert-root-prefix
# autoload -U zsh/zle/insert-root-prefix
# bindkey "^[v" zsh/zle/insert-root-prefix

# Use control-<left|right> arrows to move through words
bindkey '^[[1;5D' vi-backward-word
bindkey '^[[1;5C' vi-forward-word

# recall the last argument of the previous command, bind to Alt-p
# bindkey "\ep"  yank-last-arg
bindkey "\ep"  insert-last-word
# }}}
