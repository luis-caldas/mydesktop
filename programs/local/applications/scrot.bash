#!/usr/bin/env bash

# Globals
DEFAULT_FOLDER_NAME="scrots"
DEFAULT_SCREENSHOT_PATH="${HOME}/pics"

# Initialize var with global defaults
scrot_path="${DEFAULT_SCREENSHOT_PATH}/${DEFAULT_FOLDER_NAME}"

# Attempt to get the folder from XDG
possibly_folder="$(xdg-user-dir PICTURES 2> /dev/null)"

# Check if xdg returned anything
if [ -n "$possibly_folder" ]; then

	# Overwrite variable with the correct value
	scrot_path="${possibly_folder}/${DEFAULT_FOLDER_NAME}"

fi

# {{{ Utils

foldrizer() {
    if [ ! -d "${scrot_path}" ]; then
        mkdir -p "${scrot_path}"
    fi
}

# }}}

save() {
    # Create scrot folder
    foldrizer
    # Save screen shot
    scrot "${1}" -e "mv \$f ${scrot_path}"
}

usage() {
    echo "${0}" "{s|select|-h|--help}"
}

extra_flags=""
case $1 in
    s|select)
        extra_flags="-s"
        sleep 1
        ;;
    -h|--help)
        usage
        exit 64
        ;;
    *)
        extra_flags="-p"
        ;;
esac

save "$extra_flags"
