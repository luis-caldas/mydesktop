#!/usr/bin/env bash

# {{{ Utils

# Covers the string with given chars
cover() {
	cleft="["
	cright="]"
	printf "%c%s%c" "$cleft" "$*" "$cright"
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

clight() {
	backlight_result="$(backlight)"
	if [ -n "$backlight_result" ]; then
		cover "$backlight_result"
	fi
}

# Pretty print for bar
plight() {
	backlight_result="$(backlight)"
	if [ -n "$backlight_result" ]; then

		# Import styling tools
		folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"
		source "${folder_now}/../visual/xmobar-style.bash"

		# Build the full block
		build_block "popneobrightness" " ${backlight_result} " " ﯦ "
	fi
}

all() {
	# Run printing functions
	light="$(clight)"

	# Start array that will contain all prints
	all_prints=()

	# Add clight if preset
	if [ -n "$light" ]; then
		all_prints+=("$light")
	fi

	# Add newline if there is anything to print
	if [ ! "${#all_prints[@]}" -eq 0 ]; then
		echo "${all_prints[*]}"" "
	# If there is nothing to show throw an error
	else
		exit 1
	fi
}

# }}}
# {{{ Main

usage() {
	echo "Usage: $0 {light,backlight,pretty,all}"
}

case "$1" in
	light)
		backlight
		;;
	backlight)
		clight
		;;
	pretty)
		plight
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
