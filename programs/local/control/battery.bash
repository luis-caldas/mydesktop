#!/usr/bin/env bash

# {{{ Globals

# Warning popup variables
BATT_WARNING_PERCENTAGE=25
BATT_POP_COOLDOWN_TIME=60  # Seconds
BATT_POPED_CHECK="${HOME}/.cache/batt-pop"

# Battery paths
BATTERY_PATH="/sys/class/power_supply"
BATTERY_LIST=( "BAT" "BAT0" "BAT1" "BAT2" "cw2015-battery" )

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

# Checks which battery is present in the system from the list
which_battery() {
	for each in "${BATTERY_LIST[@]}"; do
		full_bat_path="${BATTERY_PATH}/${each}"
		if [ -d "$full_bat_path" ]; then
			echo "$full_bat_path"
		fi
	done
}

# Before anything we must check which battery is present
battery_present=$(which_battery)

# }}}
# {{{ Battery

bat_capacity() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		# Iterate through the list of the batteries
		capacity_file="$battery_now""/capacity"
		if [ -e "$capacity_file" ]; then
			xargs printf "%3s" < "$capacity_file"
		fi
	fi
}

bat_time() {
	battery_now="$1"
	if [ -n "$battery_now" ]; then
		time_file="$battery_now""/time_to_empty_now"
		if [ -e "$time_file" ]; then
			time_mins=$(cat "$time_file")
			converted_time=$(mins_to_time "$time_mins")
			printf "%5s" "$converted_time"
		fi
	fi
}

bat_charging() {
	battery_now="$1"
	if [ -n "$battery_now" ]; then
		charge_file="$battery_now""/status"
		if [ -e "$charge_file" ]; then
			if grep -Fxq "Charging" "$charge_file"; then
				echo -n c
			elif grep -Fxq "Full" "$charge_file"; then
				echo -n f
			elif grep -Fxq "Discharging" "$charge_file"; then
				echo -n d
			else
				echo -n ?
			fi
		fi
	fi
}

# }}}
# {{{ Warning

warning() {

	# Get the timestamp now
	timestamp_now="$(date +%s)"

	# Get previous timestamp
	# If it exists and is within time dont pop anything
	if [ -f "$BATT_POPED_CHECK" ]; then

		# Extract before time with the file
		time_before="$(cat "$BATT_POPED_CHECK")"

		# Check if time has not expired
		if (( timestamp_now < (time_before + BATT_POP_COOLDOWN_TIME) )); then
			return
		fi

	fi

	# Update the timer it has been shown
	echo "$timestamp_now" > "$BATT_POPED_CHECK"

	# Show the popup
	"${folder_now}/../notifications/popup-battery.bash"

}

# }}}
# {{{ Envelope

fn_all_bats() {
	if [ -n "$battery_present" ]; then

		# Get the function name
		fn_name="$1"

		# Iterate the batteries
		while IFS= read -r line; do

			# Execute the function for each battery
			"$fn_name" "$line"

			# Separate with new lines
			echo

		done <<< "$battery_present"

	fi
}

# }}}
# {{{ Printing

pretty_section() {
	source "${folder_now}/../visual/xmobar-style.bash"
	build_block "popneobattery" "${1}" " ${2}"
}

# Covers the string with given chars
cover() {
	cleft="["
	cright="]"
	printf "%c%s%c" "$cleft" "$*" "$cright"
}

pretty_icon() {
	if   [ "${1}" == "c" ]; then echo "<fn=1> </fn>"
	elif [ "${1}" == "d" ]; then echo "<fn=1> </fn>"
	elif [ "${1}" == "f" ]; then echo "<fn=1> </fn>"
	else echo "<fn=1> </fn>"
	fi
}

text_icon() {
	if   [ "${1}" == "c" ]; then echo "/\\"
	elif [ "${1}" == "d" ]; then echo "\\/"
	elif [ "${1}" == "f" ]; then echo "##"
	else echo "??"
	fi
}

power() {

	if [ -n "$battery_present" ]; then

		# Get all the batteries
		all_bats=$battery_present

		# Variable for warning popup
		is_under="yes"

		# Count battery numbers
		battery_nr=1

		# Iterate the batteries
		while IFS= read -r each_bat; do

			# Extract all possible battery information
			var_bat_capacity="$(bat_capacity "$each_bat")"
			var_bat_time="$(bat_time "$each_bat")"
			var_bat_charging="$(bat_charging "$each_bat")"

			# Chose the type of printing
			if [ "${1}" == "p" ]; then
				charging_icon="$(pretty_icon "${var_bat_charging}")"
			else
				charging_icon="$(text_icon "${var_bat_charging}")"
			fi

			# Create the array with each data
			var_bat_array=( "$var_bat_capacity" "$var_bat_time" "$charging_icon" )

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
				if [ "${1}" == "p" ]; then
					full_bat="${var_bat_real[*]}"
					pretty_section "$full_bat" "<fn=0>$(echo "${battery_nr}" | sed -e 'y|0123456789|⁰¹²³⁴⁵⁶⁷⁸⁹|')</fn>"
				else
					cover "${var_bat_real[@]}"
					# Add new line at the end
					echo
				fi
			fi

			# Check if all batteries are under a certain level
			# if so raise the battery warning

			# Check specific condition to trigger a battery warning
			# Battery must be discharging and less than a given number
			if (( "${var_bat_capacity}" <= "$BATT_WARNING_PERCENTAGE" )); then
				if [ "${var_bat_charging}" = "c" ]; then
					is_under="no"
				fi
			else
				is_under="no"
			fi

			# Update battery number
			battery_nr=$(( battery_nr + 1 ))

		done <<< "$all_bats"

		# Check if battery is under the given amount and trigger
		# warning if it is
		if [ "$is_under" == "yes" ]; then
			warning &
		fi

	fi

}

all() {
	# Run printing functions
	powers="$(power "${1}")"

	# Start array that will contain all prints
	all_prints=()

	# Add each successful
	while IFS= read -r each; do
		if [ -n "$each" ]; then
			all_prints+=("$each")
		fi
	done <<< "$powers"

	# Add newline if there is anything to print
	if [ ! "${#all_prints[@]}" -eq 0 ]; then
		if [ "${1}" == "p" ]; then space=""; else space=" "; fi
		echo "${all_prints[*]}${space}"
	# If there is nothing to show throw an error
	else
		exit 1
	fi
}

# }}}
# {{{ Main

usage() {
	echo "Usage: $0 {capacity,time,charging,power,ppower,all,pall}"
}

case "$1" in
	capacity)
		fn_all_bats bat_capacity
		;;
	time)
		fn_all_bats bat_time
		;;
	charging)
		fn_all_bats bat_charging
		;;
	power)
		power
		;;
	ppower)
		power p
		;;
	pall)
		all p
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
