#!/usr/bin/env bash

SIZE=500

#############
# Functions #
#############

function extract() {
	data=$(xrdb -query | grep "dunst\.${1}" | cut -f 2)
	[ -z "$data" ] && data="0"
	echo -n "$data"
}

########
# Main #
########

border=$(extract "border")
spacing=$(extract "space")
padding=$(extract "padding")
colour_border=$(extract "border-colour")
colour_foreground=$(extract "foreground")
colour_lbackground=$(extract "lbackground")
colour_nbackground=$(extract "nbackground")
colour_cbackground=$(extract "cbackground")

# run dunst with my custom config
dunst \
	-follow mouse \
	-geometry "${SIZE}x${SIZE}-${spacing}+${spacing}" \
	-sort \
	-indicate_hidden \
	-word_wrap \
	-padding "${padding}" \
	-horizontal_padding "${padding}" \
	-startup_notification \
	-stack_duplicates \
	-frame_width "${border}" \
	-frame_color "${colour_border}" \
	-lb "${colour_lbackground}" -lf "${colour_foreground}" \
	-nb "${colour_nbackground}" -nf "${colour_foreground}" \
	-cb "${colour_cbackground}" -cf "${colour_foreground}" \
	-sep_height "${border}"
