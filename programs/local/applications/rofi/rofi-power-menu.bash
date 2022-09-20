#!/usr/bin/env bash

# Get local folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Extract rofi variables
source "${folder_now}/extract-scale.bash"

# Create entries for dmenu
declare -A options
options=(
	["Power Off"]="systemctl poweroff"
	["Reboot "]="systemctl reboot"
	["Logout"]="pkill xmonad"
	["Close"]="exit 0"
)

# Organize and sort the options for rofi
sorted_options="$(printf "%s\n" "${!options[@]}" | awk '{ print length, $0 }' | sort -n -s -r | cut -d" " -f2-)"
IFS=$'\n' read -r -d '' -a sorted_options <<< "${sorted_options}"
IFS=','
echo_line="${sorted_options[*]}"
unset IFS

# Run rofi with exported theme file
chosen=$(echo "${echo_line}" | rofi -dmenu -sep "," \
	-theme <(envsubst < "${folder_now}/theme.rasi") \
	-theme-str "inputbar { enabled: false; } listview { lines: ${#options[@]}; }" \
	-location 0 \
	-dpi "$dpi" \
	-no-click-to-exit \
	-disable-history \
	"$@"
	)

# Execute chosen option
[ -n "${chosen}" ] && eval "${options["$chosen"]}"
