#!/usr/bin/env bash

# Get local folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Source the rofi configs
source "${folder_now}/extract-scale.bash"

# Run rofi with exported theme file
rofi -show run -display-run "$user_char " \
	-theme <(envsubst < "${folder_now}/theme.rasi") \
	-location 0 \
	-dpi "$dpi" \
	-no-click-to-exit \
	-disable-history \
	"$@"
