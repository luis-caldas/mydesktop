#!/usr/bin/env bash

# Get local folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Source the styling funcitons
source "${folder_now}/../visual/xmobar-style.bash"

# Get volume
volume="$(pamixer --get-volume)"

# Get mute
mute="$(pamixer --get-mute)"

# Check if there is volume
[ -z "${volume}" ] && { echo "<fc=${foreground},${colour2}:0><fn=1> ﳠ </fn></fc>"; exit 0; }

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
