#!/usr/bin/env bash

POPUP_ID="30083"

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

# Create icon name
function gen_icon_name () {
	"${folder_now}/colour_icon.bash" "media-playback-${1}.svg"
}

# Get the default media status
function get_status() {
	playerctl status
}

# Create the notification function
function notfy() {
	dunstify -i "$(gen_icon_name "${1}")" "${2}" -r "${POPUP_ID}" -t 1500
}

function main() {

	# Get the status
	status_now=$(get_status)

	# Check which status it is and set icon
	icon="stop"
	message="Stopped"
	if [ "${1}" == "pp" ]; then
		icon="start"
		message="Playing"
		if [ "$status_now" == "Playing" ]; then
			icon="pause"
			message="Paused"
		fi
	fi

	# Send the notification
	notfy "${icon}" "${message}"

}

main "$@"
