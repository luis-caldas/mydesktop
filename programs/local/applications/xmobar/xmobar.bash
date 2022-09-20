#!/usr/bin/env bash

########
# Main #
########

# Get local folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Source the styling funcitons
source "${folder_now}/../../visual/xmobar-style.bash"

# Create line for top bar
top_bar="<action=\`wneocalendar\`><fc=${background},${colour0}:0> %date% </fc></action>"
top_bar+="$(colour_arrow r "${colour0},${colour1}")"
top_bar+="$(colour_arrow r "${colour1},${colour2}")"
top_bar+="$(colour_arrow r "${colour2},${background}")"
top_bar+=" }{ "
top_bar+="$(colour_arrow r "${background},${colour2}")"
top_bar+="$(colour_arrow r "${colour2},${colour1}")"
top_bar+="%lock-show%"
top_bar+="%control-bluetooth%"
top_bar+="$(build_block "wneovolume" " %volumm% " "  ")"
#top_bar+="$(build_block "" " %mpris2% " "  ")"
top_bar+="%control-bat%"
top_bar+="%control-light%"
top_bar+="$(build_block "popneonetwork" " %interf% " "  ")"
top_bar+="$(build_block "wneosysinfo" " %cpu% " " ﬙ ")"
top_bar+="%cmd-gov%"
top_bar+="$(build_block "wneosysinfo" " %memory% " "  " "e")"

# Create bottom bar
bottom_bar="%UnsafeStdinReader%"
bottom_bar+="<fc=${colour2},${colour2}:0> </fc>"
bottom_bar+="$(colour_arrow r "${colour2},${background}")"
bottom_bar+="}{"
bottom_bar+="$(colour_arrow l "${colour2},${background}")"
bottom_bar+="$(simple_block "neorofidumb" "<fn=1>   </fn>")"
bottom_bar+="$(simple_block "clipster -s -c" "<fn=1>   </fn>")"
bottom_bar+="$(simple_block "wneonetwork" "<fn=1>   </fn>")"
bottom_bar+="$(simple_block "neopowermenu" "<fn=1>   </fn>")"
bottom_bar+="$(colour_arrow r "${colour2},${background}")"

# Export needed variables
export -- \
	alpha background foreground \
	colour0 colour1 colour2 \
	top_bar bottom_bar

# Run both xmobar instances
xmobar "$@" <(envsubst < "${folder_now}/top.xmobarrc") &
xmobar "$@" <(envsubst < "${folder_now}/bottom.xmobarrc") < /dev/stdin &

# Wait for both xmobar instances
wait
