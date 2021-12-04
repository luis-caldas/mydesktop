#!/usr/bin/env bash

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

########
# Main #
########

# Get local folder
folder_now="$(get_folder)"

# Source the styling funcitons
source "${folder_now}/../../visual/xmobar-style.bash"

# Create line for top bar
top_bar="<action=\`wneocalendar\`><fc=${background},${colour1}:0> %date% </fc></action>"
top_bar+="$(colour_arrow r "${colour1},${colour2}")"
top_bar+="$(colour_arrow r "${colour2},${background}")"
top_bar+=" }{ "
top_bar+="$(colour_arrow r "${background},${colour2}")"
top_bar+="$(colour_arrow r "${colour2},${colour1}")"
top_bar+="%lock-show%"
top_bar+="$(build_block "popneovolume" "%default:Master%" "")"
top_bar+="$(build_block "popneobattery" "%control-bat%" "")"
top_bar+="%control-light%"
top_bar+="$(build_block "wneoweather" "%EICK%" "")"
top_bar+="$(build_block "wneonetwork" "%interf%" "")"
top_bar+="$(build_block "wneosysinfo" "%cpu%" "")"
top_bar+="%cmd-gov%"
top_bar+="$(build_block "wneosysinfo" "%memory%" "" "e")"

echo "$top_bar"

bottom_bar="%UnsafeStdinReader%$(colour_arrow r "${colour2},${background}")}{"

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
