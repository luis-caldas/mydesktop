#!/usr/bin/env bash

# my keyboard remappings
# needed for a more hhkb like experience

# transform caps into ctrl
setxkbmap -option ctrl:nocaps

# swap backspace and the backslash
xmodmap -e "keycode 51 = BackSpace"
xmodmap -e "keycode 22 = backslash bar"
