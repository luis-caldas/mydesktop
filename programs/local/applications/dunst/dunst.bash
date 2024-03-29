#!/usr/bin/env bash

SIZE=500
XRESOURCE_NAME="dunst"

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

function extract() {
	data=$(xrdb -query | grep "${XRESOURCE_NAME}\.${1}:" | cut -f 2)
	[ -z "$data" ] && data="0"
	echo -n "$data"
}

scaling_factor=$(extract_scaling_factor)

function scale_defloat() {
	scaled=$(echo "$scaling_factor""*""${1}" | bc)
	echo "${scaled%.*}"
}

function x_scale_defloat() {
	extracted="$(extract "${1}")"
	scale_defloat "$extracted"
}

########
# Main #
########

width="${SIZE}"
border=$(extract "border-size")
scale="${scaling_factor}"
space=$(x_scale_defloat "space")
padding=$(extract "padding")
alpha=$(extract "alpha")
min_icon_size=$(extract "min-icon-size")
max_icon_size=$(extract "max-icon-size")
bar_height=$(extract "bar-size")
bar_frame=$(extract "bar-frame")
colour_border=$(extract "border-colour")
colour_foreground=$(extract "foreground")
colour_lbackground=$(extract "lbackground")
colour_nbackground=$(extract "nbackground")
colour_cbackground=$(extract "cbackground")

# Get local folder
local_folder="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Export variables that are going to be used in envsubst
export -- scale \
	width border space padding min_icon_size max_icon_size \
	bar_height bar_frame \
	alpha colour_border colour_foreground \
	colour_lbackground colour_nbackground colour_cbackground

# Start the program with custom rc file
envsubst < "${local_folder}/dunstrc" # | dunst -config -
envsubst < "${local_folder}/dunstrc" | dunst -config -
