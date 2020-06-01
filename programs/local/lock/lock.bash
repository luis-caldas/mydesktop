#!/usr/bin/env bash

TIMEOUT=10 # seconds

# blank screen
xset dpms force off

# set dpms timeout to someting fast
xset s "$TIMEOUT"

# lock screen
slock

# restore the timeout to nothing
xset s off
