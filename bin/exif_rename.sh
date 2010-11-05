#!/bin/sh
# rename files from exif info into format YYYY-MM-DD-###.jpg
# where ### is number between 001 and 999
# required: jhead, sed, grep, dirname, mv
# johniez, http://johniez.com
# init version, no safety controls - backup your data before ;)

PHOTO_COUNT=0
LAST_DATE=0

if [ $# -lt 1 ]; then
    echo "Error: missing arguments"
    echo "Usage: $0 file1.jpg file2.jpg .."
    exit 2
fi

for i in $*; do
    NEW_DATE=`jhead "$i" | grep 'File date' | sed -e 's/.*: \(.*\)$/\1/' | tr -s ":" "-" | tr -s "[:blank:]" "_"`

    # if [ "$NEW_DATE" == "$LAST_DATE" ]; then
    #     PHOTO_COUNT=$((PHOTO_COUNT+1))
    # else
    #     PHOTO_COUNT=1
    #     LAST_DATE=$NEW_DATE
    # fi

    DIR=`dirname "$i"`

    # if [ $PHOTO_COUNT -lt 10 ]; then
    #     mv "$i" "${DIR}/${NEW_DATE}-00${PHOTO_COUNT}.jpg"
    # elif [ $PHOTO_COUNT -lt 100 ]; then
    #     mv "$i" "${DIR}/${NEW_DATE}-0${PHOTO_COUNT}.jpg"
    # else
    #     mv "$i" "${DIR}/${NEW_DATE}-${PHOTO_COUNT}.jpg"
    # fi

    mv "$i" "${DIR}/${NEW_DATE}.jpg"
done
