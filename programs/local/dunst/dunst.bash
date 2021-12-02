#!/usr/bin/env bash

SIZE=500

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
	data=$(xrdb -query | grep "dunst\.${1}" | cut -f 2)
	[ -z "$data" ] && data="0"
	echo -n "$data"
}

########
# Main #
########

scaling_factor=$(extract_scaling_factor)

border=$(extract "border-size")
spacing=$(extract "space")
padding=$(extract "padding")
alpha=$(extract "alpha")
min_icon_size=$(extract "min-icon-size")
colour_border=$(extract "border-colour")
colour_foreground=$(extract "foreground")
colour_lbackground=$(extract "lbackground")
colour_nbackground=$(extract "nbackground")
colour_cbackground=$(extract "cbackground")

# Calculate the width and dpi with the scaling factor
new_float_width=$(echo "$scaling_factor""*""$SIZE" | bc)
new_float_border=$(echo "$scaling_factor""*""$border" | bc)
new_float_space=$(echo "$scaling_factor""*""$spacing" | bc)
new_float_padding=$(echo "$scaling_factor""*""$padding" | bc)
new_float_min_icon_size=$(echo "$scaling_factor""*""$min_icon_size" | bc)

# Defloat variables
new_width=${new_float_width%.*}
new_border=${new_float_border%.*}
new_space=${new_float_space%.*}
new_padding=${new_float_padding%.*}
new_min_icon_size=${new_float_min_icon_size%.*}

# Get local folder
local_folder="$(get_folder)"

# Export variables that are going to be used in envsubst
export new_width new_border new_space new_padding new_min_icon_size \
	alpha colour_border colour_foreground \
	colour_lbackground colour_nbackground colour_cbackground

# Start the program with custom rc file
envsubst < "${local_folder}/dunstrc" | dunst -config -
