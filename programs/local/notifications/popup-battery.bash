#!/usr/bin/env bash

# Get icon depending on number
function get_icon() {

	# Reasign variable in
	number="$1"

	# Limit number boundaries
	if [ "$number" -gt 100 ]; then number=100; fi
	if [ "$number" -lt 0   ]; then number=0  ; fi

	# Get ratio between number and list batteries
	battery_ratio=$(awk -v n="$number" 'BEGIN{print int((n+5)/10) * 10}')

	# Catch case of full battery
	if [ "$battery_ratio" -ge 100 ]; then battery_ratio=90; fi

	# Get the filename of icon
	filename="battery-level-${battery_ratio}.svg"

	# Get filename from colour changed
	"${folder_now}/generate-icon.bash" "${filename}"

}

# Transform capacity name to equivalent number
function cap_equivalence() {
	case "${1}" in
		"Unknown") 	echo 0	;;
		"Critical") echo 5	;;
		"Low") 		echo 15	;;
		"Normal") 	echo 50	;;
		"High") 	echo 80	;;
		"Full") 	echo 100;;
		*)			echo 0	;;
	esac
}

function add_separator() {
	separator="$1"
	all_args=( "$@" )
	args_array=()
	for each_arg in "${all_args[@]}"; do
		[ -n "$each_arg" ] && args_array+=("$each_arg")
	done
	for ((i = 1; i < ${#args_array[@]}; i++)); do
		sep_now="$separator"
		(( i == (${#args_array[@]} - 1) )) && sep_now='\n'
		printf "%s%s" "${args_array[$i]}" "$sep_now"
	done
}

# Get our folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Get battery now
all_bats="$("${folder_now}/../control/battery.bash" extra)"

# Iterate all batteries
while IFS= read -r each_bat; do

	# Extract array data
	args_array=()
	while IFS= read -r each_line; do
		args_array+=("$each_line")
	done < <(xargs printf "%s\n" <<< "$each_bat")

	# Check if battery has at least a capacity level
	bat_capacity="${args_array[1]}"
	bat_level="${args_array[2]}"
	[ -z "$bat_capacity" ] && [ -z "$bat_level" ] && continue

	# Check if batt has number otherwise use level
	if [ -z "$bat_capacity" ]; then
		bat_percent="$(cap_equivalence "$bat_level")"
	else
		bat_percent="$bat_capacity"
	fi

	# Create title
	battery_title="$(printf "%s" "${args_array[0]}")"

	# Variable reasignment
	bat_status="${args_array[3]}"
	bat_time="${args_array[4]}"
	bat_cycle="${args_array[5]}"
	bat_voltage="${args_array[6]}"
	read -ra energy_array <<< "${args_array[7]}"
	bat_energy="${energy_array[0]}"
	bat_energy_design="${energy_array[1]}"

	# Body parts
	percent_indicator="\n@ ${bat_percent}%"
	[ -z "$bat_capacity" ] && percent_indicator="\n@ ${bat_level}"
	percent_bar=( -h "int:value:${bat_percent}" )
	[ -z "$bat_capacity" ] && percent_bar=()

	str_time=""
	[ -n "$bat_time" ] && str_time="$bat_time Left"
	str_cycles=""
	[ -n "$bat_cycles" ] && str_cycles="$bat_cycle Cycles"
	str_voltage=""
	[ -n "$bat_voltage" ] && str_voltage="$bat_voltage Volts\n"
	str_energy=""
	[ -n "$bat_energy" ] && [ -n "$bat_energy_design" ] && str_energy="$bat_energy Wh / $bat_energy_design Wh"

	# Join time and status
	str_time_status="$(add_separator " - " "$bat_status" "$str_time")"
	str_other="$(add_separator " @ " "$str_energy" "$str_cycles")"

	# Join full body message
	full_body="\n${str_time_status}${str_voltage}${str_other}${percent_indicator}"

	# Send notification
	icon_path="$(get_icon "$bat_percent")"
	dunstify \
		-i "${icon_path}" \
		"${percent_bar[@]}" \
		"${battery_title}" \
		"${full_body}"

done <<< "$all_bats"
