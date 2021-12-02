#!/usr/bin/env bash

SIZE=500
XRESOURCE_NAME="dunst"

#############
# Functions #
#############

# Function to get real script dir
function get_folder() {

    # get the folder in which the script is located
    SOURCE="${BASH_SOURCE[0]}"

    # resolve $SOURCE until the file is no longer a symlink
    while [ -h "$SOURCE" ]; do

      DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

      SOURCE="$(readlink "$SOURCE")"

      # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
      [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"

    done

    # the final assignment of the directory
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

    # return the directory
    echo "$DIR"
}

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

width=$(scale_defloat "${SIZE}")
border=$(x_scale_defloat "border-size")
space=$(x_scale_defloat "space")
padding=$(x_scale_defloat "padding")
alpha=$(extract "alpha")
min_icon_size=$(x_scale_defloat "min-icon-size")
bar_height=$(x_scale_defloat "bar-size")
bar_frame=$(x_scale_defloat "bar-frame")
colour_border=$(extract "border-colour")
colour_foreground=$(extract "foreground")
colour_lbackground=$(extract "lbackground")
colour_nbackground=$(extract "nbackground")
colour_cbackground=$(extract "cbackground")

# Get local folder
local_folder="$(get_folder)"

# Export variables that are going to be used in envsubst
export width border space padding min_icon_size \
	bar_height bar_frame \
	alpha colour_border colour_foreground \
	colour_lbackground colour_nbackground colour_cbackground

# Start the program with custom rc file
envsubst < "${local_folder}/dunstrc" | dunst -config -
