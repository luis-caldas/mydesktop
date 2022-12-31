#!/usr/bin/env bash

# {{{ Globals

# Blue folder
BLUE_CACHE_FOLDER="${HOME}/.cache/neoblue"

# Warning popup variables
BLUE_WARNING_PERCENTAGE=25
BLUE_POP_COOLDOWN_TIME=60  # Seconds

# Cache variables
BLUE_READ_COOLDOWN=$(( 60 * 2 )) # Seconds

# State of folder creation
cache_created=0

# }}}
# {{{ Utils

folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Creates the cache folder
check_cache_create() {
	if [ "$cache_created" -eq 0 ]; then # Check wether it was created by this same script
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

# Get full dbus bluez object
bluez_full() {
	dbus-send \
		--system \
		--print-reply \
		--type=method_call \
		--dest='org.bluez' '/' \
		org.freedesktop.DBus.ObjectManager.GetManagedObjects \
		| grep "object path" \
		| sed -e 's/variant//' \
			-e 's/object path//' \
			-e 's/^\s*//' \
			-e 's/^"//' \
			-e 's/"$//' \
		| sort -h \
		| uniq
}

# Find adapter from mac
find_path_bluez_dbus() {

	# Extract the wanted device
	grep "${1}" <<< "${2}" | head -n1

}

# }}}
# {{{ Devices

devices() {

	# Create array for all devices
	all_connected_devices=()

	# Check if the bluetooth service is running
	if ! systemctl is-active --quiet bluetooth &&
	   ! systemctl is-active --quiet bluetoothd ; then
	   return
	fi

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

		# Print the list of connected devices
		printf "%s\n" "${all_connected_devices[@]}"

	fi

}

# }}}
# {{{ Battery

check_battery_rfcomm() {

	while true; do
		# Run command and save both stdout and stderr
		{
			IFS=$'\n' read -r -d '' stderr_now;
			IFS=$'\n' read -r -d '' stdout_now;
		} < <((printf '\0%s\0' "$(bluetooth_battery "$1")" 1>&2) 2>&1)

		# Check for output on stdout
		output_value="$(awk '{print $NF}' <<< "${stdout_now}" | sed 's/[^0-9]*//g')"

		# Check if device busy was the cause
		if [ -z "$output_value" ]; then
			if ! grep -q 'Device or resource busy' <<< "$stderr_now"; then
				break
			fi
		else
			echo "${output_value}"
			break
		fi

	done

}

check_battery_dbus() {

	# Fix given mac
	fixed_mac="$(fix_mac "${1}")"

	# Find which adapter is connected to device
	dev_path="$(find_path_bluez_dbus "${fixed_mac}" "${2}")"

	# Get the battery percentage
	dbus-send \
		--print-reply=literal \
		--system \
		--dest=org.bluez \
		"${dev_path}" \
		org.freedesktop.DBus.Properties.Get \
		string:"org.bluez.Battery1" \
		string:"Percentage" 2> /dev/null | awk '{print $NF}'

}

check_battery() {

	# Print battery by whatever method necessary
	rf_bat="$(check_battery_rfcomm "${1}")"
	db_bat="$(check_battery_dbus "${1}" "${2}")"

	# Create print number
	print_nr=0

	if [ -n "$rf_bat" ]; then
		print_nr="$rf_bat"
	elif [ -n "$db_bat" ]; then
		print_nr="$db_bat"
	else
		printf "?\n"
		return
	fi

	printf "%3d\n" "$print_nr"

}

main_check() {

	# Create cache for dbus bluez
	bluez_cache="$(bluez_full)"

	# Iterate the devices
	while read -r each_dev; do

		# Clear variable for next iteration
		batt_now=""

		# Create fixed mac for it and cache name
		fixed_mac="$(fix_mac "${each_dev}")"
		cache_name="batt-check-${fixed_mac}"
		cache_path="${BLUE_CACHE_FOLDER}/${cache_name}"

		# Check if current device is cached and requires to be reevaluated
		timestamp_now="$(date +%s)"
		if [ -f "${cache_path}" ]; then
			# Get file
			cache_file="$(cat "${cache_path}")"
			# Get time within file
			time_before="$(awk '{print $1}' <<< "${cache_file}")"
			# Check if time has not expired
			if (( timestamp_now < (time_before + BLUE_READ_COOLDOWN) )); then
				batt_now="$(awk '{print $2}' <<< "${cache_file}")"
			fi
		fi

		# Check if cache was not successful
		if [ -z "$batt_now" ]; then

			# Check battery of current device
			batt_now="$(check_battery "${each_dev}" "${bluez_cache}")"

			# Check cache folder
			check_cache_create

			# Get updated timestamp
			timestamp_updated="$(date +%s)"

			# Save to cache the new information
			printf "%s %s\n" "${timestamp_updated}" "${batt_now}" > "${cache_path}"

		fi

		# Check if only a print is sufficient
		if [ "${2}" == "print" ]; then
			printf "%s %s\n" "${each_dev}" "${batt_now}"
			continue
		fi

		# Check if no percentage was found
		if [ "${batt_now}" == "?" ]; then
			continue
		fi

		# Check battery percentage
		if (( "${batt_now}" <= "${BLUE_WARNING_PERCENTAGE}" )); then
			warning "${each_dev}" "${batt_now}" &
		fi

	done <<< "${1}"

}

# }}}
# {{{ Warning

warning() {

	# Get the timestamp now
	timestamp_now="$(date +%s)"

	# Create specific string for device
	fixed_mac="$(fix_mac "${1}")"
	pop_name="pop-${fixed_mac}"
	pop_path="${BLUE_CACHE_FOLDER}/${pop_name}"

	# Get previous timestamp
	# If it exists and is within time dont pop anything
	if [ -f "$pop_path" ]; then

		# Extract before time with the file
		time_before="$(cat "$pop_path")"

		# Check if time has not expired
		if (( timestamp_now < (time_before + BLUE_POP_COOLDOWN_TIME) )); then
			return
		fi

	fi

	# Update the timer it has been shown
	check_cache_create
	echo "$timestamp_now" > "$pop_path"

	# Show the popup
	"${folder_now}/../notifications/popup-bluetooth.bash" single "${1}" "${2}"

}

# }}}
# {{{ Printing

pretty_section() {
	source "${folder_now}/../visual/xmobar-style.bash"
	build_block "popneoblue" " ${1} " "  "
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

# Only print devices and percentages (api)
print_devices() {
	devices_now="$(devices)"
	main_check "${devices_now}" print
}

# }}}
# {{{ Main

usage() {
	echo "Usage: $0 {pretty,simple,devices}"
}

case "$1" in
	pretty)
		print pretty
		;;
	simple)
		print
		;;
	devices)
		print_devices
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
