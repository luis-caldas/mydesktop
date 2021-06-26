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
batt_now="$("$folder_now""/bat_light.bash" capacity | xargs)"
read -ra arg_array <<< "$batt_now"

# Run the command for the batteries
result="$("$folder_now""/bat_warning.bash" "${arg_array[@]}")"

# Count number of lines
lines_total="$(wc -l <<< "$result")"

# Add extra lines for cursor
full_lines=$(( lines_total + 3 + 3 ))

# Launches a st with given size and the command inside
st \
	-T "neobatt" \
	-g 70x"$full_lines" \
	-f mono:size=12 \
	-e sh \
	-c "echo \"${result}\" && read"
