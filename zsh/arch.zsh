#!/usr/bin/env zsh
# Archlinux configuration

export PACMAN=pacman-color

# search
function pacs {
echo -e "$(pacman -Ss $@ | sed \
     -e 's,^testing/.*,\\033[1;31m&\\033[0;0m,g' \
     -e 's,^current/.*,\\033[1;32m&\\033[0;0m,g' \
     -e 's,^extra/.*,\\033[1;35m&\\033[0;0m,g' \
     -e 's,^community/.*,\\033[1;33m&\\033[0;0m,g' \
     -e 's,^[a-z]*/.*,\\033[1;34m&\\033[0;0m,g')"
}

# update
function pacup {
   sudo pacman-color -Syu
}
