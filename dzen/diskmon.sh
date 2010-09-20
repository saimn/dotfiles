#!/bin/zsh
# Disk usage monitor using dzen
# needs gawk
# (c) Tom Rauchenwald

source $HOME/.dzen/dzconf.sh

FN=$barXFont
BG=$colorBG
FG=$colorFG
W=100         #bar width
X=1024        #x position
Y=768         #y position
GH=7          #gauge height
GW=50         #gauge width
GFG=$colorFG5 #gauge color
GBG=$colorFG2 #gauge background


gawk "
BEGIN {
    CMD=\"${gdbarcmd} -w $GW -h $GH -fg '$GFG' -bg '$GBG'\"
    while(1) {
        i=1
        while ((\"df\" | getline ) > 0)
        { if (\$0 ~ /^\//) {
                print \$5 |& CMD
                CMD |& getline lin
                if (i == 1)
                    print \"^cs()\\n^tw()\", \$6, lin
                else
                    print \$6, lin, \"  \"
                i++
                close(CMD)
            }
        }
        close(\"df\")
        system(\"sleep 5\")
    }
}" | dzen2 -ta c -sa r -l 5 -w $W -tw $W -x $X -y $Y -fg $FG -bg $BG -fn $FN -e "button1=togglecollapse;button3=exit"


##!/bin/zsh
## (c) 2007 Robert Manea
##
 
#typeset -A disks
## Format: label1 mountpoint1 label2 mountpoint2 ... labelN mountpointN
#disks=(root / home /home usr /usr)
 
## Label color
#LCOLOR=white
## gauge height
#GH=7
## gauge width
#GW=70
## gauge fg color
#GFG='#aecf96'
##gauge bg color
#GBG='#494b4f'
 
#get_disk_usage() {
        #local rstr; local tstr; local i
 
        #for i in ${(k)disks}
        #{
                #tstr=$(print `df -h $disks[$i]|sed -ne 's/^.* \([0-9]*\)% .*/\1/p'` 100 | \
                       #gdbar -h $GH -w $GW -fg $GFG -bg $GBG -l "^fg(${LCOLOR})${i}^fg()" -nonl)
                #rstr=${rstr}|>' '${tstr}|>
        #}
 
        #print -n $rstr
#}
