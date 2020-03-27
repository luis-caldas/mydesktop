#!/bin/bash

# argument 1 is the percentage to be shown
# argument 2 is the text to be shown on the top of the slider

# globals
DEFAULT_FONT="-*-*-medium-r-*-*-20-120-*-*-*-*-*-*"
DEFAULT_FONT="10x20"

# kill all previous instances of osd_cat that are running
killall -9 -q osd_cat &>/dev/null

# my custom osd cat
osd_cat -p bottom -o "300" -A center \
    -f "$DEFAULT_FONT" \
    -O 1 \
    -d 3 \
    -c white \
    -b percentage \
    -P "$1" \
    -T "$2"
