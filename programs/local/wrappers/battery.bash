#!/usr/bin/env bash

# Get our folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Get battery now
batt_now="$("${folder_now}/../control/battery.bash" capacity | xargs)"
read -ra arg_array <<< "$batt_now"

# Run the command for the batteries
result="$("${folder_now}/../visual/battery-ascii-creator.bash" "${arg_array[@]}")"

# Count number of lines
lines_total="$(wc -l <<< "$result")"

# Add extra lines for cursor
full_lines=$(( lines_total + 3 + 3 ))

# Launches a st with given size and the command inside
st \
	-T "wneobattery" \
	-g 70x"$full_lines" \
	-f mono:size=12 \
	-e sh \
	-c "echo \"${result}\" && read"
