#!/usr/bin/env bash

# Get our folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Create icon name
function gen_icon_name () {
	"${folder_now}/generate-icon.bash" "${1}"
}

# Create the notification function
function notfy() {
	dunstify -i "$(gen_icon_name "${1}")" "${2}" "\n${3}" -t 1500
}

function main() {
	# Send the notification
	notfy "${1}" "${2}" "${3}"
}

main "$@"
