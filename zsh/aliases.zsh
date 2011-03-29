#!/usr/bin/env zsh

# {{{ Aliases

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ] || [ -x /bin/dircolors ]; then
  if [[ -r ~/.dir_colors ]]; then
    eval `dircolors -b ~/.dir_colors`
  else
    eval `dircolors -b`
  fi

  alias ls='ls -F --color=auto'
  alias dir='dir --color=auto'
  #alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  #alias fgrep='fgrep --color=auto'
  #alias egrep='egrep --color=auto'
fi

# Quelques options de ls :
# -F: Avoir des / après les noms de dossiers > plus de couleur
# -G: cacher les noms de groupe

alias ll='ls -l --group-directories-first'  # listage de répertoire détaillé
alias lh='ll -Gh'                           # idem avec tailles human
alias lt='ll -tr'                           # tri par date de modif
alias la='ls -la'
alias u="cd .. && ls"
alias lsd='ls -ld *(-/DN)'

# -p : conserve les dates, droits lors de la copie
alias cp='cp -p'
#alias mv='mv -v'
#alias rm='rm -v'

alias du='du -h --max-depth=1'
alias dusort='du -x --block-size=1048576 | sort -nr'
alias df='df -h'

alias :e="\$EDITOR"
alias :wq='exit'
alias :q!='exit'
alias bashfr="lynx --dump --display_charset=utf8 \"http://danstonchat.com/random.html\" | awk '\$1~\"#\" && \$0!~\"RSS\" { getline; while (\$1!~\"#\") { print \$0; getline;}; exit}'"
alias top-10="sed -e 's/sudo //' $HOME/.zsh/history |  cut -d' ' -f1 | sort | uniq -c | sort -rg | head"

# per extentions
alias -s pdf="zathura"
alias -s ps="evince"
alias -s png="mirage"
alias -s jpg="mirage"
alias -s log="tail -f"
alias -s conf="vim"

# misc commands
alias ht='urxvt -name htop -title htop +sb -geometry 80x6 -e htop &'
alias firedev='firefox -no-remote -P Webdev'
alias svnd='svn diff --diff-cmd "~/bin/svnvimdiff"'
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'

alias e="emacsclient"
alias ec="emacsclient -n -c"
alias et="emacsclient -t"
alias v='exec urxvtc -e "bash" -c "exec vim $@" >> /dev/null &'
alias vi="vim"
alias sv='exec urxvtc -e "bash" -c "exec sudo vim $@" >> /dev/null &'

# simple webserver on port 8000
alias webshare='python -m SimpleHTTPServer'

# open & close dropbox coffre
#alias sync="~/.dropbox-dist/dropboxd &"
alias coffre-open='encfs $HOME/Dropbox/coffre $HOME/Documents/coffre-dropbox'
alias coffre-close='fusermount -u $HOME/Documents/coffre-dropbox'

# ping (since control-c don't work for break ping)
#alias ping="ping -c 3"

# start mutt with list mailboxes
#[[ -x `which mutt` ]] && alias mutt="mutt -y"

# }}}

