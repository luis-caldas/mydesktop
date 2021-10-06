#!/usr/bin/env bash

########
# Vars #
########

DEFAULT_WIDTH=750
DEFAULT_DPI=100

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

function extract_border() {
	border=$(xrdb -query | grep "rofi\.border" | cut -f 2)
	[ -z "$border" ] && border=1
	echo -n "$border"
}

function extract_space() {
	space=$(xrdb -query | grep "rofi\.space" | cut -f 2)
	[ -z "$space" ] && space=5
	echo -n "$space"
}

function extract_linemargin() {
	line_marging=$(xrdb -query | grep "rofi\.line-margin" | cut -f 2)
	[ -z "$line_marging" ] && line_marging=2
	echo -n "$line_marging"
}

function extract_linepadding() {
	line_padding=$(xrdb -query | grep "rofi\.line-padding" | cut -f 2)
	[ -z "$line_padding" ] && line_padding=8
	echo -n "$line_padding"
}

function extract_lines() {
	lines=$(xrdb -query | grep "rofi\.lines" | cut -f 2)
	[ -z "$lines" ] && lines=10
	echo -n "$lines"
}

########
# Main #
########

# get the gnomes scaling factor if possible
scaling_factor=$(extract_scaling_factor)
border=$(extract_border)
spacing=$(extract_space)
line_marging=$(extract_linemargin)
line_padding=$(extract_linepadding)
lines=$(extract_lines)

# calculate the width and dpi with the scaling factor
new_float_width=$(echo "$scaling_factor""*""$DEFAULT_WIDTH" | bc)
new_float_dpi=$(echo "$scaling_factor""*""$DEFAULT_DPI" | bc)
new_float_border=$(echo "$scaling_factor""*""$border" | bc)
new_float_space=$(echo "$scaling_factor""*""$spacing" | bc)
new_float_linemargin=$(echo "$scaling_factor""*""$line_marging" | bc)
new_float_linepadding=$(echo "$scaling_factor""*""$line_padding" | bc)
new_width=${new_float_width%.*}
new_dpi=${new_float_dpi%.*}
new_border=${new_float_border%.*}
new_space=${new_float_space%.*}
new_line_margin=${new_float_linemargin%.*}
new_line_padding=${new_float_linepadding%.*}

# get the proper char for the user running
[ "$EUID" -ne 0 ] && user_char="$" || user_char="#"

# run rofi with our configs
rofi -show run -display-run "$user_char " --location 0 \
	-padding "$new_space" \
	-width "$new_width" \
	-lines "$lines" \
	-line-margin "$new_line_margin" -line-padding "$new_line_padding" \
	-separator-style none -columns 1 \
	-bw "$new_border" \
	-dpi "$new_dpi" \
	-no-click-to-exit \
	-disable-history \
	-hide-scrollbar \
	-kb-row-select "Tab" -kb-row-tab ""
