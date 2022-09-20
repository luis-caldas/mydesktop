#!/usr/bin/env bash

ICON_NAME="display-brightness.svg"
POPUP_ID="30081"

# Get our folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

function main() {

	# Get light
	light_now=$("${folder_now}/../control/light.bash" light)

	# Check if not empty
	if [ -n "$light_now" ]; then

		# Get correct colour of icon
		icon=$("${folder_now}/generate-icon.bash" "${ICON_NAME}")

		# If not empty send notification
		dunstify -i "${icon}" -h "int:value:${light_now}" "Brightness" "\n${light_now} %" -r "${POPUP_ID}" -t 1500

	else
		exit 1
	fi

}

main "$@"
