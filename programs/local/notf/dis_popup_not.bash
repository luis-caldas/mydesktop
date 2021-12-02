#!/usr/bin/env bash

ICON_NAME="display-brightness.svg"
POPUP_ID="30081"

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

function main() {

	# Get light
	light_now=$("${folder_now}/../control/light.bash" light)

	# Check if not empty
	if [ -n "$light_now" ]; then

		# Get correct colour of icon
		icon=$("${folder_now}/colour_icon.bash" "${ICON_NAME}")

		# If not empty send notification
		dunstify -i "${icon}" -h "int:value:${light_now}" "Brightness - ${light_now} %" -r "${POPUP_ID}" -t 1500

	else
		exit 1
	fi

}

main "$@"
