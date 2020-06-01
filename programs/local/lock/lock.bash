#!/usr/bin/env bash

TIMEOUT=10 # seconds

# blank screen
xset dpms force off

# set dpms timeout to someting fast
xset dpms "$TIMEOUT" "$TIMEOUT" "$TIMEOUT"

# lock screen
slock

# restore the timeout to nothing
xset dpms 0 0 0 
