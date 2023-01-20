#!/usr/bin/env bash

#############
# Functions #
#############

function extract_scaling_factor() {

	# run the gsettings command to get the scaling factor
	factor=$(gsettings get org.gnome.desktop.interface scaling-factor 2>/dev/null || false)

	# if the variable is not empty, extract its value, if not assume the scaling is one
	if [ -n "$factor" ]; then
	factor=$(echo "$factor" | awk '{print $NF}')
	fi

	# check if a few sys vars exist
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$GDK_SCALE"
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$ELM_SCALE"
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$QT_AUTO_SCREEN_SCALE_FACTOR"

	# if nothing was set until now
	# set it to one
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="1"

	# return the scaling factor
	echo -n "$factor"

}

function scale_defloat() {
	scaled=$(echo "$scaling_factor""*""${1}" | bc)
	echo "${scaled%.*}"
}

########
# Main #
########

# Get local folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Get the scaling factor
scaling_factor=$(extract_scaling_factor)

# Source the styling funcitons
source "${folder_now}/../../visual/xmobar-style.bash"

# Extract the xmobar height and scale it
unscaled_bar_height=$(extract "height")
bar_height=$(scale_defloat "$unscaled_bar_height")

# Create line for top bar
top_bar="<action=\`wneocalendar\`><fc=${background},${colour0}:0> %date% </fc></action>"
top_bar+="$(colour_arrow r "${colour0},${colour1}")"
top_bar+="$(colour_arrow r "${colour1},${colour2}")"
top_bar+="$(colour_arrow r "${colour2},${background}")"
top_bar+=" }{ "
top_bar+="$(colour_arrow l "${colour2},${background}")"
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
	top_bar bottom_bar \
	bar_height

# Run both xmobar instances
xmobar "$@" <(envsubst < "${folder_now}/top.xmobarrc") &
xmobar "$@" <(envsubst < "${folder_now}/bottom.xmobarrc") < /dev/stdin &

# Wait for both xmobar instances
wait
