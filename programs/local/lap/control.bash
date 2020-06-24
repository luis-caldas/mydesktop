#!/usr/bin/env bash

# {{{ Globals

BATTERY_PATH="/sys/class/power_supply"
BATTERY_LIST=( "BAT" "BAT0" "BAT1" "cw2015-battery" )

# }}}
# {{{ Utils

# Transforms minutes to 
mins_to_time() {
	# Time divider
	divider=":"

	# Calculate hours and mins
	((hours=${1}/60))
	((minutes=${1}%60))

	# Return the formatted hour
	printf "%2d%c%02d" "$hours" "$divider" "$minutes"
}

# Covers the string with given chars
cover() {
	cleft="["
	cright="]"
	printf "%c%s%c" "$cleft" "$*" "$cright"
}

# Checks which battery is present in the system from the list
which_battery() {
	for each in "${BATTERY_LIST[@]}"; do
		full_bat_path="${BATTERY_PATH}/${each}"
		if [ -d "$full_bat_path" ]; then
			echo "$full_bat_path"
			break
		fi
	done
}

# Before anything we must check which battery is present
battery_present=$(which_battery)

# }}}
# {{{ Battery

bat_capacity() {
	# Check the battery present
	if [ -n "$battery_present" ]; then 
		capacity_file="$battery_present""/capacity"
		if [ -e "$capacity_file" ]; then
			xargs printf "%3s" < "$capacity_file"
		fi
	fi
}

bat_time() {
	if [ -n "$battery_present" ]; then
		time_file="$battery_present""/time_to_empty_now"
		if [ -e "$time_file" ]; then
			time_mins=$(cat "$time_file")
			converted_time=$(mins_to_time "$time_mins")
			printf "%5s" "$converted_time"
		fi
	fi
}

bat_charging() {
	if [ -n "$battery_present" ]; then
		charge_file="$battery_present""/status"
		if [ -e "$charge_file" ]; then
			if grep -Fxq "Charging" "$charge_file"; then
				printf "%s" "/\\"
			else
				printf "%s" "\\/"
			fi
		fi
	fi
}

# }}}
# {{{ Backlight

backlight() {
	# Check if light throws an error (no backlight found)
	if ! { light 2>&1 >&3 3>&- | grep '^' >&2; } 3>&1 3>/dev/null 2>/dev/null; then
		light=$(light)
		printf "%3.0f" "$light"
	fi
}

# }}}
# {{{ Printing

power() {

	if [ -n "$battery_present" ]; then

		# Extract all possible battery information
		var_bat_array=( "$(bat_capacity)" "$(bat_time)" "$(bat_charging)" )

		# Create the array that will contain the existing vars
		var_bat_real=()

		# Check if each exist
		for each in "${var_bat_array[@]}"; do
			if [ -n "$each" ]; then
				var_bat_real+=("$each")
			fi
		done
		
		# Check if the array is not empty (no battery info found)
		if [ ! "${#var_bat_real[@]}" -eq 0 ]; then
			cover "${var_bat_real[@]}"
		fi

	fi

}

clight() {
	backlight_result="$(backlight)"
	if [ -n "$backlight_result" ]; then
		cover "$backlight_result"
	fi
}

all() {
	# Run printing functions
	prints=( "$(power)" "$(clight)" )

	# Start array that will contain all prints
	all_prints=()

	# Add each successful
	for each in "${prints[@]}"; do
		if [ -n "$each" ]; then
			all_prints+=("$each")
		fi
	done

	# Add newline if there is anything to print
	if [ ! "${#all_prints[@]}" -eq 0 ]; then
		echo "${all_prints[@]} "
	# If there is nothing to show throw an error
	else
		exit 1
	fi
}

# }}}
# {{{ Main

usage() {
	echo "Usage: $0 {capacity,time,charging,light,power,backlight,all}"
}

case "$1" in
	capacity)
		bat_capacity
		;;
	time)
		bat_time
		;;
	charging)
		bat_charging
		;;
	light)
		backlight
		;;
	power)
		power
		;;
	backlight)
		clight
		;;
	all)
		all
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
