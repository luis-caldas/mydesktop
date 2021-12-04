#!/usr/bin/env bash

########
# Vars #
########

XRESOURCE_NAME="xmobar"

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

# Get local folder
folder_now="$(get_folder)"

# Extract all from xresources

# Extract colours xresource
alpha=$(extract "alpha")
background=$(extract "background")
foreground=$(extract "foreground")
colour0=$(extract "colour0")
colour1=$(extract "colour1")
colour2=$(extract "colour2")

# Create some variables used in the bars
left_arrow=""
right_arrow=""
colour_arrow() {
	[ "${1}" == "l" ] && arrow="${left_arrow}" || arrow="${right_arrow}"
	echo "<fc=${2}:0><fn=2>${arrow}</fn></fc>"
}

build_block () {
	[ "${4}" == "e" ] && init_arrow="<fc=${foreground},${colour2}> </fc>" || init_arrow="$(colour_arrow r "${colour2},${colour1}")"
	block="<action=\`${1}\`><fc=${background},${colour1}:0><fn=1> ${3} </fn></fc>"
	block+="$(colour_arrow r "${colour1},${colour2}")<fc=${foreground},${colour2}:0> ${2} </fc></action>${init_arrow}"
	echo "${block}"
}

# Create line for top bar
top_bar="<action=\`neocalendar\`><fc=${background},${colour1}:0> %date% </fc></action>$(colour_arrow r "$colour1")"
top_bar+=" }{ "
top_bar+="$(colour_arrow r "${background},${colour1}")"
top_bar+="$(build_block "a" "%lock-show%" "")"
top_bar+="$(build_block "neovol" "%default:Master%" "")"
top_bar+="$(build_block "batt-show" "%control-bat%" "")"
top_bar+="$(build_block "light-show" "%control-light%" "")"
top_bar+="$(build_block "neoweather" "%EICK%" "")"
top_bar+="$(build_block "neonet" "%interf%" "")"
top_bar+="$(build_block "neosysinfo" "%cpu%" "")"
top_bar+="$(build_block "neosysinfo" "%cmd-gov%" "")"
top_bar+="$(build_block "neosysinfo" "%memory% " "" "e")"

echo "$top_bar"

bottom_bar=" %UnsafeStdinReader% }{"

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
