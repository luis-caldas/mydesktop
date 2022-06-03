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

	# Empty vars
	batt_str=""
	batt_loader=()

	# Check if there is battery
	if [ ! "${batt}" == "?" ]; then
		batt_str="$(printf "\n\nBattery @ %s%%" "${batt}")"
		batt_loader=( "-h" "int:value:${batt}" )
	fi

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
	text_now="$(printf "%s - %s\n\n%s%s" "${description}" "${trusted}" "${mac_addr}" "${batt_str}")"
	dunstify -i "${icon_path}" "${batt_loader[@]}" "${dev_name}" "\n${text_now}"

}

all() {

	# Get all devices
	all_dev_info="$("${folder_now}/../control/bluetooth.bash" devices)"

	# Iterate and notify
	while read -r each_dev; do
		# Split input into vars
		mac_addr="$(awk '{print $1}' <<< "${each_dev}")"
		battery="$(awk '{print $2}' <<< "${each_dev}")"
		single "${mac_addr}" "${battery}"
	done <<< "${all_dev_info}"

}

usage() {
	echo "Usage: $0 {single [device mac] [percentage]}"
}

case "$1" in
	single)
		single "${2}" "${3}"
		;;
	-h|--help)
		usage
		exit 64
		;;
	*)
		all
		;;
esac
