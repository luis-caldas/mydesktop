#!/usr/bin/env bash

TOKEN="#PLEASECHANGEME"
CACHE_DIR="/tmp/cached-notification-icons"
XRES_NAME="dunst"

# Get our folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

main () {

	# Extract file name from input
	file_name_input="$1"
	file_name_only=$(basename -- "$file_name_input")

	# Check if directory exists and if not create it
	[ ! -d "${CACHE_DIR}" ] && mkdir -p "${CACHE_DIR}"

	# extract colour from xrdb
	foreground_colour=$(xrdb -query | grep "${XRES_NAME}\.foreground" | cut -f 2)
	colour_name=$(tr -d "#" <<< "${foreground_colour}")

	# Clean file input for names
	file_ext="${file_name_only##*.}"
	file_name_no_ext="${file_name_only%.*}"

	# Generate filename
	generated_file_name="${CACHE_DIR}/${file_name_no_ext}-cache-${colour_name}.${file_ext}"

	# Check if file is cached
	if [ -f "${generated_file_name}" ]; then
		echo "${generated_file_name}"
	else

		# Create new icon to a cache directory
		sed -e "s/${TOKEN}/${foreground_colour}/g" < "${folder_now}/icons/${file_name_input}" > "${generated_file_name}"

		# Return the proper filename
		echo "${generated_file_name}"

	fi

}

main "$@"
