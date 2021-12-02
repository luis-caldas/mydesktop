#!/usr/bin/env bash

########
# Vars #
########

DEFAULT_WIDTH=750
DEFAULT_DPI=100
XRESOURCE_NAME="rofi"

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
	data=$(xrdb -query | grep "${XRESOURCE_NAME}\.${1}" | cut -f 2)
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

# Extract all from xresources
width=$(scale_defloat "$DEFAULT_WIDTH")
dpi=$(scale_defloat "$DEFAULT_DPI")
border=$(x_scale_defloat "border")
space=$(x_scale_defloat "space")
line_margin=$(x_scale_defloat "line-margin")
line_padding=$(x_scale_defloat "line-padding")
lines=$(extract "lines")
colour_w=$(extract "color-window")
colour_n=$(extract "color-normal")
colour_a=$(extract "color-active")
colour_u=$(extract "color-urgent")

# get the proper char for the user running
[ "$EUID" -ne 0 ] && user_char="$" || user_char="#"

# run rofi with our configs
echo rofi -show run -display-run "$user_char " \
	-location 0 \
	-padding "$space" \
	-width "$width" \
	-lines "$lines" \
	-line-margin "$line_margin" -line-padding "$line_padding" \
	-separator-style none \
	-columns 1 \
	-bw "$border" \
	-dpi "$dpi" \
	-no-click-to-exit \
	-disable-history \
	-hide-scrollbar \
	-kb-row-select "Tab" -kb-row-tab "" \
	-color-window "$colour_w" \
	-color-normal "$colour_n" \
	-color-active "$colour_a" \
	-color-urgent "$colour_u" \
	"$@"
