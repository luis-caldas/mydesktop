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

# Get local folder
folder_now="$(get_folder)"

# Source the styling funcitons
source "${folder_now}/../visual/xmobar-style.bash"

# Get volume
volume="$(pamixer --get-volume)"

# Get mute
mute="$(pamixer --get-mute)"

# Check if there is volume
[ -z "${volume}" ] && exit 1

simple() {

	# Create mute string
	mute_string=""
	[ "${mute}" == "true" ] && mute_string=" M "

	printit "${mute_string}"

}

pretty() {

	# Create mute string
	mute_string=""
	[ "${mute}" == "true" ] && mute_string="<fc=${foreground},${colour2}:0><fn=1>  </fn></fc>"

	printit "${mute_string}"

}

printit() {
	printf "%3d%s" "${volume}" "${1}"
}

usage() {
	echo "Usage: $0 {simple}"
}

case "$1" in
	simple)
		simple
		;;
	-h|--help)
		usage
		exit 64
		;;
	*)
		pretty
		;;
esac

# }}}
