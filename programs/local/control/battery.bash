#!/usr/bin/env bash

# {{{ Globals

# Warning popup variables
BATT_WARNING_PERCENTAGE=25
BATT_POP_COOLDOWN_TIME=60  # Seconds
BATT_POPED_CHECK="${HOME}/.cache/batt-pop"

# Battery paths
BATTERY_PATH="/sys/class/power_supply"

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
	printf "%d%c%02d" "$hours" "$divider" "$minutes"
}

# Checks which battery is present in the system from the list
which_battery() {
	for each in "$BATTERY_PATH"/*; do
		type_file_path="${each}/type"
		if grep -Fxq "Battery" "$type_file_path"; then
			echo "$each"
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
		capacity_file="$battery_now""/capacity"
		if [ -e "$capacity_file" ]; then
			xargs printf "%3s" < "$capacity_file"
		fi
	fi
}

bat_level() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		level_file="$battery_now""/capacity_level"
		if [ -e "$level_file" ]; then
			cat "$level_file"
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
			printf "%s" "$converted_time"
		fi
	fi
}

bat_cycle() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		cycle_file="$battery_now""/cycle_count"
		if [ -e "$cycle_file" ]; then
			cat "$cycle_file"
		fi
	fi
}

bat_tech() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		tech_file="$battery_now""/technology"
		if [ -e "$tech_file" ]; then
			cat "$tech_file"
		fi
	fi
}

bat_name() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		model_file="$battery_now""/model_name"
		if [ -e "$model_file" ]; then
			cat < "$model_file"
		fi
	fi
}

bat_voltage() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		voltage_file="$battery_now""/voltage_now"
		if [ -e "$voltage_file" ]; then
			awk '{ print $1 / 1000000 }' < "$voltage_file" | xargs printf "%s"
		fi
	fi
}

bat_energy() {
	battery_now="$1"
	# Check battery present
	if [ -n "$battery_now" ]; then
		energy_full_file="$battery_now""/energy_full"
		energy_full_design_file="$battery_now""/energy_full_design"
		if [ -e "$energy_full_file" ] && [ -e "$energy_full_design_file" ]; then
			awk '{ print $1 / 1000000 }' < "$energy_full_file" | xargs printf "%s "
			awk '{ print $1 / 1000000 }' < "$energy_full_design_file" | xargs printf "%s"
		fi
	fi
}

bat_status() {
	battery_now="$1"
	if [ -n "$battery_now" ]; then
		charge_file="$battery_now""/status"
		if [ -e "$charge_file" ]; then
			cat "$charge_file"
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
	build_block "popneobattery" " ${1}" "  ${2}"
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

extra() {

	if [ -n "$battery_present" ]; then

		# Get all the batteries
		all_bats=$battery_present

		# Count battery numbers
		battery_nr=1

		# Create array with the functions
		all_funcs=( "bat_name" "bat_capacity" "bat_level" "bat_status" "bat_time" "bat_cycle" "bat_voltage" "bat_energy" )

		# Iterate the batteries
		while IFS= read -r each_bat; do
			# Create array with all conatined data
			all_response=()
			for each_func in "${all_funcs[@]}"; do
				result="$("$each_func" "$each_bat")"
				if [ -n "$result" ]; then
					all_response+=("$result")
				else
					all_response+=("")
				fi
			done
			# Print info
			echo "${all_response[@]@Q}"
		done <<< "$all_bats"
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
			var_bat_level="$(bat_level "$each_bat")"
			var_bat_time="$(bat_time "$each_bat")"
			var_bat_charging="$(bat_charging "$each_bat")"

			# Chose the type of printing
			if [ "${1}" == "p" ]; then
				charging_icon="$(pretty_icon "${var_bat_charging}")"
			else
				charging_icon="$(text_icon "${var_bat_charging}")"
			fi

			# Check which type of capacity is to be shown
			show_cap="$var_bat_capacity"
			[ -z "$var_bat_capacity" ] && show_cap="$var_bat_level"

			# Create the array with each data
			var_bat_array=( "$show_cap" "$var_bat_time" "$charging_icon" )

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

			if [ -z "$var_bat_capacity" ]; then
				if [ "$var_bat_level" == "Critical" ]; then
					if [ "${var_bat_charging}" = "c" ]; then
						is_under="no"
					fi
				else
					is_under="no"
				fi
			else
				# Check specific condition to trigger a battery warning
				# Battery must be discharging and less than a given number
				if (( "${var_bat_capacity}" <= "$BATT_WARNING_PERCENTAGE" )); then
					if [ "${var_bat_charging}" = "c" ]; then
						is_under="no"
					fi
				else
					is_under="no"
				fi
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
	echo "Usage: $0 {capacity,extra,power,ppower,all,pall}"
}

case "$1" in
	capacity)
		fn_all_bats bat_capacity
		;;
	extra)
		extra
		;;
	power)
		power
		;;
	ppower)
		power p
		;;
	all)
		all
		;;
	pall)
		all p
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
