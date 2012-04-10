# -*- shell-script -*-
# vim: set syntax=zsh:

function big() { find $1 -type f -printf "%k %p\n" | sort -rn | head }
function recent() { find $1 -mtime -1 -print }
function calc(){ awk "BEGIN{ print $* }" ;}
function dsync () { rsync -lprt --progress --stats --delete "$1/" "$2/" }
function findnosecure () { find / -perm +2000 -o -perm +4000 -print 2>/dev/null }

function mdv() { mplayer -dvd-device $1 dvd://$2 }
function rmdv() { for i in `seq 1 1 30` ; mplayer -dvd-device $1 dvd://$i }
function vplay() { quvi $1 -f best --exec "mplayer %u" }

function webmenc { ffmpeg -i $1 -vcodec libvpx -acodec libvorbis -threads 4 $1.webm }
function mp4enc { ffmpeg -i $1 -vcodec libx264 -acodec libmp3lame -preset slow -crf 22 -threads 0 $1.mp4 }

function v() { urxvtc -e "bash" -c "exec vim $@" >> /dev/null & }
function sv() { urxvtc -e "bash" -c "exec sudo vim $@" >> /dev/null & }

#function cd () {
#if [[ -z $2 ]]; then
  #if [[ -f $1 ]]; then
    #builtin cd $1:h
  #else
    #builtin cd $1
  #fi
#else
  #if [[ -z $3 ]]; then
    #builtin cd $1 $2
  #else
    #echo cd: too many arguments
  #fi
#fi
#}

# update screen caption, use the last argument as hostname
#function ssh () {
    #target=$_
    #target=${target//*@/}
    #set_title $target
    #command ssh $*
#}

function utf8 () {
    if [[ $COLORTERM == "gnome-terminal" ]]; then
	    echo -e '\e%G'
    fi
    export LC_CTYPE=fr_FR.UTF8
    unalias vi
    if [[ ! -x $(which -p vim) ]] ; then
	    echo "utf8() Warning, vim not found !"
    else
	    alias vi='vim "+set enc=utf8"'
    fi
}

# Convert * to mp3 files
function 2mp3()
{
  until [ -z $1 ]
  do
    ffmpeg -i $1 -acodec libmp3lame "`basename $1`.mp3"
    shift
  done
}

function 2html()
{
  until [ -z $1 ]
  do
    vim -n -c ':TOhtml' -c ':wqa' $1 &>/dev/null
    shift
  done
}

# Strip metadata in pdf
strip-pdf-metadata() {
   pdftk $1 dump_data | \
       sed -e 's/\(InfoValue:\)\s.*/\1\ /g' | \
       pdftk $1 update_info - output clean-$1
}

