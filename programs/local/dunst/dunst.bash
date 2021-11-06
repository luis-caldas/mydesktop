#!/usr/bin/env bash

SIZE=500

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
	data=$(xrdb -query | grep "dunst\.${1}" | cut -f 2)
	[ -z "$data" ] && data="0"
	echo -n "$data"
}

########
# Main #
########

scaling_factor=$(extract_scaling_factor)

border=$(extract "border")
spacing=$(extract "space")
padding=$(extract "padding")
colour_border=$(extract "border-colour")
colour_foreground=$(extract "foreground")
colour_lbackground=$(extract "lbackground")
colour_nbackground=$(extract "nbackground")
colour_cbackground=$(extract "cbackground")

# calculate the width and dpi with the scaling factor
new_float_width=$(echo "$scaling_factor""*""$SIZE" | bc)
new_float_border=$(echo "$scaling_factor""*""$border" | bc)
new_float_space=$(echo "$scaling_factor""*""$spacing" | bc)
new_float_padding=$(echo "$scaling_factor""*""$padding" | bc)

new_width=${new_float_width%.*}
new_border=${new_float_border%.*}
new_space=${new_float_space%.*}
new_padding=${new_float_padding%.*}

# run dunst with my custom config
dunst \
	-follow mouse \
	-geometry "${new_width}x${new_width}-${new_space}+${new_space}" \
	-sort \
	-indicate_hidden \
	-word_wrap \
	-padding "${new_padding}" \
	-horizontal_padding "${new_padding}" \
	-stack_duplicates \
	-frame_width "${new_border}" \
	-frame_color "${colour_border}" \
	-lb "${colour_lbackground}" -lf "${colour_foreground}" \
	-nb "${colour_nbackground}" -nf "${colour_foreground}" \
	-cb "${colour_cbackground}" -cf "${colour_foreground}" \
	-sep_height "${new_border}"

