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
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

alias e="emacsclient"
alias g='git'
alias j='fasd_cd -i'
alias m='fasd -f -e mplayer' # quick opening files with mplayer
alias o='fasd -a -e xdg-open' # quick opening files with xdg-open
alias t='todo.sh'
alias u="cd .. && ls"
alias v='fasd -f -e vim' # quick opening files with vim
# alias v="vim"
alias z='fasd_cd -d'

alias ll='ls -l --group-directories-first'
alias l='ll -Gh'
alias lh='ll -Gh'
alias lt='ll -tr'
alias la='ls -la'
alias lsd='ls -ld *(-/DN)'

# -p : conserve les dates, droits lors de la copie
alias cp='cp -p'
alias rm='trash-put'
alias du='du -h --max-depth=1'
alias dusort='du -h --max-depth=1 | sort -h -r'
alias df='df -h'
alias psg="ps auxw | grep -i "

# per extentions
alias -s conf="vim"
alias -s fits="ds9"
alias -s html="firefox -new-tab"
alias -s jpg="eog"
alias -s log="less"
alias -s pdf="evince"
alias -s png="eog"
alias -s ps="evince"
alias -s fits="ds9"

# misc commands
alias ack=ag
alias dif='colordiff -u'
alias chromium='chromium --disk-cache-dir=/tmp/chromium-cache'
alias ff='firefox -new-tab'
alias firedef='firefox -no-remote -P default'
alias firedev='firefox -no-remote -P Webdev'
alias ht='urxvt -name htop -title htop +sb -geometry 80x6 -e htop &'
alias ipyqt='ipython2 qtconsole --pylab'
alias wo='workon'
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'

alias ec="emacsclient -n -c"
alias et="emacsclient -t"
alias vi="vim"
alias sv="sudo vim"

alias cup='conda clean -py;conda update --all'

# alias less="/usr/share/vim/vim73/macros/less.sh"

# simple webserver on port 8000
alias webshare='python2 -m SimpleHTTPServer'

# open & close dropbox coffre
# alias sync="unison default -batch"
# alias coffre-open='encfs $HOME/Dropbox/coffre $HOME/Documents/coffre-dropbox'
# alias coffre-close='fusermount -u $HOME/Documents/coffre-dropbox'

# ping (since control-c don't work for break ping)
alias ping="ping -c 3"

# start mutt with list mailboxes
#[[ -x `which mutt` ]] && alias mutt="mutt -y"

alias weechat="urxvt -icon $HOME/lib/dotfiles/applications/weechat-64x64.png -T Weechat -name weechat -e weechat &"
alias mutt="urxvt -icon $HOME/lib/dotfiles/applications/mutt.svg -T Mutt -name Mutt -e mutt &"

# git
alias gb='git branch'
alias gba='git branch -a -vv'

alias gc='git commit -v'
alias gca='git commit -v -a'
alias gcm='git checkout master'
alias gco='git checkout'

alias gdi='git diff'
alias ggrep='git grep --color -n -P'
alias gh='git hist'
alias glgg='git log --graph --max-count=5'
alias glg='git log --stat --max-count=5'
alias gpull='git pull'
alias gpush='git push'
alias gst='git status -sb'

# }}}
