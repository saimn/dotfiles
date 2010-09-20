#!/bin/zsh
#
# xmonad statusline, (c) 2007 by Robert Manea
#

source $HOME/.dzen/dzconf.sh

colorBarBG=$colorFG
colorBarFG=$colorFG5
barWidth=60
barHeight=7

# Configuration
date_format='%a %d %b'
time_format='%H:%M'
# time_zones=(France/Paris)
maildir=$HOME/Mail

# Main loop interval in seconds
interval=1

# function calling intervals in seconds
cputempival=60
dateival=60
mailival=60
memfreeival=60
# weatherival=1800

# Functions
fdate() {
    date +$date_format
}
ftime() {
    date +$time_format
}

# fgtime() {
#     local i
#     for i in $time_zones
#         { print -n "${i:t}:" $(TZ=$i date +'%H:%M')' ' }
# }

fcputemp() {
   print -n ${(@)$(</proc/acpi/thermal_zone/TZ00/temperature)[2,3]} "/ "
   print -n ${(@)$(</proc/acpi/thermal_zone/TZ01/temperature)[2,3]} "/ "
   print -n ${(@)$(</proc/acpi/thermal_zone/TZ02/temperature)[2,3]}
}

fmail() {
    local -A counts; local i

    for i in "${maildir:-${HOME}/Mail}"/**/new/*
        { (( counts[${i:h:h:t}]++ )) }
    for i in ${(k)counts}
        { print -n $i: $counts[$i]' ' }
}

# fweather() {
#     /path/to/dzenWeather.pl
# }


fvolume() {
    if [ "$HOST" = "fireball" ]; then
        percentage=`amixer |grep -A 6 \'Master\' |awk {'print $5'} |grep -m 1 % |sed -e 's/[][%]//g'`
    else
        percentage=`amixer |grep -A 6 \'Master\' |awk {'print $4'} |grep -m 1 % |sed -e 's/[][%]//g'`
    fi

    ismute=`amixer |grep -A 6 \'Master\'|awk {'print $6'} |grep -m 1 "\[[on|off]" | sed -e 's/[][]//g'`

    if [[ $ismute == "off" ]]; then
        print -n "$(echo "0" | ${gdbarcmd} -fg ${colorBarFG} -bg ${colorBarBG} -h ${barHeight} -w ${barWidth})"
    else
        print -n "$(echo $percentage | ${gdbarcmd} -fg ${colorBarFG} -bg ${colorBarBG} -h ${barHeight} -w ${barWidth})"
    fi
}

fmemfree() {
    AWKS='/MemTotal/   {mtotal=$2};
          /MemFree/    {mfree=$2};
          END {print mtotal-mfree " " mtotal; }'

    paste -d ' ' - <(awk "$AWKS" /proc/meminfo | \
        ${gdbarcmd} -fg ${colorBarFG} -bg ${colorBarBG} -w ${barWidth} -h ${barHeight})
}

fcpubar() {
    ${gcpubarcmd} -fg ${colorBarFG} -bg ${colorBarBG} -h ${barHeight} -w ${barWidth} -c 3 -i 0.2  | tail -1
}
# Main

# initialize data
cputempcounter=$cputempival
datecounter=$dateival
mailcounter=$mailival
memfreecounter=$memfreeival
# weathercounter=$weatherival

while true; do
   if [ $datecounter -ge $dateival ]; then
     pdate=$(fdate)
     ptime=$(ftime)
     datecounter=0
   fi

   if [ $mailcounter -ge $mailival ]; then
     tmail=$(fmail)
       if [ $tmail ]; then
         pmail="^fg(${colorFG4})^i(${dzen_iconpath}/envelope.xbm)^p(3)${tmail}^fg()"
       else
         pmail="^i(${dzen_iconpath}/envelope.xbm)"
       fi
     mailcounter=0
   fi

   if [ $cputempcounter -ge $cputempival ]; then
     pcputemp=$(fcputemp)
     cputempcounter=0
   fi

   if [ $memfreecounter -ge $memfreeival ]; then
       pmemfree="^fg(${colorBarFG})^i(${dzen_iconpath}/mem.xbm)$(fmemfree)^fg()"
       memfreecounter=0
   fi

   pvolume="^fg(${colorBarFG})^i(${dzen_iconpath}/volume.xbm)$(fvolume)^fg()"

   pcpubar="^fg(${colorBarFG})^i(${dzen_iconpath}/cpu.xbm)$(fcpubar)^fg()"

   # if [ $weathercounter -ge $weatherival ]; then
   #   pweather=$(fweather)
   #   weathercounter=0
   # fi

   # arrange and print the status line
   #print "$pweather $pcputemp $pgtime $pmail ^fg(white)${pdate}^fg()"
   pcommon="^fg(${colorFG4})${pdate} ^fg(${colorBarFG})${ptime}^fg() $pmail $pmemfree $pcpubar $pvolume"

   if  [ "$HOST" = "devenson" ]; then
       print "${pcputemp} ${pcommon}"
   else
       print "${pcommon}"
   fi

   datecounter=$((datecounter+1))
   mailcounter=$((mailcounter+1))
   gtimecounter=$((gtimecounter+1))
   cputempcounter=$((cputempcounter+1))
   #weathercounter=$((weathercounter+1))

   sleep $interval
done

#status.sh | dzen2 -ta r -fn '-*-profont-*-*-*-*-11-*-*-*-*-*-iso8859' -bg '#2c2c32' -fg 'grey70' -p -e '' &

