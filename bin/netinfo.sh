#!/bin/bash
# sudo apt install dnsutils jq && sudo -H pip3 install speedtest-cli

myip=$(dig +short myip.opendns.com @resolver1.opendns.com)
geoip=$(curl -s ipinfo.io/"$myip" | jq -r '[.country, .city] | join(", ")')

echo "$myip ($geoip)"
speedtest-cli --no-upload --simple --secure

exit 0
