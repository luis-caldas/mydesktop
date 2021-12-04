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

# Get our folder
folder_now="$(get_folder)"

# Get battery now
batt_now="$("${folder_now}/../control/battery.bash" capacity | xargs)"
read -ra arg_array <<< "$batt_now"

# Battery counter
batt_count=1

# Iterate the array of batteries
for each_batt in "${arg_array[@]}"; do
	icon_path=$(get_icon "$each_batt")
	dunstify -i "${icon_path}" -h "int:value:${each_batt}" "Battery - ${batt_count} @ ${each_batt}%"
	batt_count=$(( batt_count + 1 ))
done
