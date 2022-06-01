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

# Get device info
all_device_info() {
	bluetoothctl info "${1}" | grep 'Name\|Icon\|Trusted'
}

single() {

	# Get device info
	all_info="$(all_device_info "${1}")"

	# Reassing names
	mac_addr="${1}"
	batt="${2}"

	# Split info into variables
	dev_name="$(grep Name <<< "${all_info}" | awk '{$1=""; print $0}' | sed -e 's/^\s*//' -e 's/\s*$//')"
	des_name="$(grep Icon <<< "${all_info}" | awk '{$1=""; print $0}' | sed -e 's/^\s*//' -e 's/\s*$//')"
	trs_name="$(grep Trusted <<< "${all_info}" | awk '{$1=""; print $0}' | sed -e 's/^\s*//' -e 's/\s*$//')"

	# Fixes strings
	trusted="Not trusted yet"
	[ "$trs_name" == "yes" ] && trusted="Trusted"
	description="$(tr '-' ' ' <<< "${des_name}" | awk '{ for(i=1;i<=NF;i++) { $i=toupper(substr($i,1,1)) substr($i,2) } } 1')"

	# Show notification
	icon_path="$("${folder_now}/generate-icon.bash" "bluetooth.svg")"
	text_now="$(printf "%s\n\n%s\n%s\n\nBattery @ %s%%" "${mac_addr}" "${description}" "${trusted}" "${batt}")"
	dunstify -i "${icon_path}" -h "int:value:${batt}" "${dev_name}" "\n${text_now}"

}

usage() {
	echo "Usage: $0 {single,all} [device mac] [percentage]"
}

case "$1" in
	single)
		single "${2}" "${3}"
		;;
	all)
		echo "not implemented"
		;;
	-h|--help)
		usage
		exit 64
		;;
	*)
		usage
		exit 1
		;;
esac
