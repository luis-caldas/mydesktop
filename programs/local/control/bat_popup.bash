#!/usr/bin/env bash

# Function to get current folder
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

# Get our folder
folder_now="$(get_folder)"

# Get battery now
batt_now="$("$folder_now""/bat_light.bash" capacity)"

# Launches a st with given size and the command inside
st \
	-T "neobatt" \
	-g 70x19 \
	-f mono:size=12 \
	-e sh \
	-c "$folder_now""/bat_warning.bash"' '"$batt_now"' && read'
