#!/usr/bin/env bash

# my keyboard remappings
# needed for a more hhkb like experience

# swap backspace and the backslash
xmodmap -e "keycode 22 = BackSpace"
xmodmap -e "keycode 51 = backslash bar"
