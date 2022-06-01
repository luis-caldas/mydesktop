#!/usr/bin/env bash

# {{{ Globals

# Blue folder
BLUE_CACHE_FOLDER="${HOME}/.cache/neoblue"

# Warning popup variables
BLUE_WARNING_PERCENTAGE=25
BLUE_POP_COOLDOWN_TIME=60  # Seconds

# Cache variables
READ_COOLDOWN=$(( 60 * 5 )) # Seconds

# State of folder creation
cache_created=0

# }}}
# {{{ Utils

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
folder_now="$(get_folder)"

# Creates the cache folder
check_cache_create() {
	if [ "$cache_created" -ne 0 ]; then # Check wether it was created by this same script
		if [ ! -d "$BLUE_CACHE_FOLDER" ]; then
			mkdir -p "${BLUE_CACHE_FOLDER}"
		fi
		cache_created=1
	fi
}

# Fixes mac addresses to a simple string
fix_mac() {
	tr '[:lower:]' '[:upper:]' <<< "${1}" | tr ':' '_'
}
revert_mac() {
	tr '_' ':' <<< "${1}"
}

# }}}
# {{{ Devices

devices() {

	# Create array for all devices
	all_connected_devices=()

	# Get every device in bluetooth
	all_devices="$(bluetoothctl devices | awk '{print $2}')"

	# Iterate devices and find connected ones
	while read -r each_mac; do
		is_conn="$(bluetoothctl info "${each_mac}" | grep Connected | awk '{print $2}')"
		# Check if device is connected
		if [ "${is_conn}" == "yes" ]; then
			all_connected_devices+=("${each_mac}")
		fi
	done <<< "${all_devices}"

	# If there are connected devices
	if [ "${#all_connected_devices[@]}" -ne 0 ]; then

		# Check for cache folder
		check_cache_create

		# Print the list of connected devices
		printf "%s\n" "${all_connected_devices[@]}"

	fi

}

# }}}
# {{{ Battery

check_battery_rfcomm() {
	bluetooth_battery "$1" 2> /dev/null | awk '{print $NF}' | sed 's/[^0-9]*//g'
}

check_battery_dbus() {
	converted_mac="$(tr ':' '_' <<< "${1}")"
	# FIXME Hardcoded bluetooth adapter number
	dbus-send \
		--print-reply=literal \
		--system \
		--dest=org.bluez /org/bluez/hci0/dev_"${converted_mac}" \
		org.freedesktop.DBus.Properties.Get \
		string:"org.bluez.Battery1" \
		string:"Percentage" 2> /dev/null | awk '{print $NF}'
}

check_battery() {
	# Print battery by whatever method necessary
	rf_bat="$(check_battery_rfcomm "${1}")"
	db_bat="$(check_battery_dbus "${1}")"

	# Create print number
	print_nr=0

	if [ -n "$rf_bat" ]; then
		print_nr="$rf_bat"
	elif [ -n "$db_bat" ]; then
		print_nr="$db_bat"
	else
		return
	fi

	printf "%s %3d\n" "$1" "$print_nr"
}

main_check() {

	# Iterate the devices
	while read -r each_dev; do

		# Check battery percentage
		if (( "${batt_now}" <= "${BLUE_WARNING_PERCENTAGE}" )); then
			warning "${each_dev}" &
		fi

	done <<< "${1}"

}

# }}}
# {{{ Warning

warning() {

	# Get the timestamp now
	timestamp_now="$(date +%s)"

	# Create specific string for device
	dev_pop_file=$()

	# Get previous timestamp
	# If it exists and is within time dont pop anything
	if [ -f "$dev_pop_file" ]; then

		# Extract before time with the file
		time_before="$(cat "$dev_pop_file")"

		# Check if time has not expired
		if (( timestamp_now < (time_before + BLUE_POP_COOLDOWN_TIME) )); then
			return
		fi

	fi

	# Update the timer it has been shown
	echo "$timestamp_now" > "$dev_pop_file"

	# Show the popup
	"${folder_now}/../notifications/popup-bluetooth.bash" "${1}"

}

# }}}
# {{{ Printing

pretty_section() {
	source "${folder_now}/../visual/xmobar-style.bash"
	build_block "popneobluetooth" " ${1} " "  "
}

# Covers the string with given chars
cover() {
	cleft="["
	cright="]"
	printf "%c%s%c" "$cleft" "$*" "$cright"
}

print() {

	# Get devices
	devices_now="$(devices)"

	# Start number of connections as zero
	nr_conn=0

	# Get number of connected devices
	if [ -n "$devices_now" ]; then
		nr_conn="$(wc -l <<< "${devices_now}")"
	fi

	# If there are connected devices print them
	if [ "${nr_conn}" -gt 0 ]; then

		# Check if pretty or simple print
		if [ "$1" == "pretty" ]; then
			pretty_section "${nr_conn}"
		else
			simple_string="$(printf "B %d" "${nr_conn}")"
			cover "${simple_string}"
		fi

		# Start the battery checking in a new subprocess
		main_check "${devices_now}" &

	fi

}

# }}}
# {{{ Main

usage() {
	echo "Usage: $0 {pretty,simple}"
}

case "$1" in
	pretty)
		print pretty
		;;
	simple)
		print
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

# }}}
