#!/usr/bin/env python2

# geoip -- Script that connects to the whatismyipaddress service for
#          finding geolocation data about a particular IP address.
# Copyright (C) 2010 Adrian C. <anrxc sysphere.org>
#           Licensed under the WTFPL


from os import path
from sys import argv
from commands import getoutput


# The browser, and user-agent to report
uagent  = "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)"
browser = {
    "links"  : ["/usr/bin/links", "-fake-user-agent '%s'" %uagent],
    "lynx"   : ["/usr/bin/lynx",  "-useragent='%s'" %uagent],
    "w3m"    : ["/usr/bin/w3m", ""],
    "elinks" : ["/usr/bin/elinks", ""],
}

# Geolocation data we are interested in
info = [
    "Hostname",  "ISP",        "Organization",
    "Proxy",     "Type",       "Assignment",
    "Blacklist", "Country",    "State/Region",
    "City",      "Latitude",   "Longitude",
    "Area Code", "Postal Code" ]


def main(client, addr):
    # All of the browsers support dumping formatted pages to stdout
    page = getoutput("%s %s -dump http://whatismyipaddress.com/ip/%s"
                     % (client[0], client[1], addr)).split("\n")

    # Crude, but not sensibly slow
    for line in page:
        for item in info:
            if line.strip().startswith(item):
                print line


if __name__ == "__main__":
    for key, value in browser.items():
        if path.isfile(value[0]):
            client = browser[key]
            break
    try:
        main(client, argv[1])
    except IndexError:
        raise SystemExit("Usage: %s {IP address}" % path.split(argv[0])[1])
    except NameError:
        raise SystemExit("Error: %s couldn't find a browser"
                         % path.split(argv[0])[1])
