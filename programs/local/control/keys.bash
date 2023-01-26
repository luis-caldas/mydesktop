#!/usr/bin/env bash

# Shows lock keys status

#############
# Functions #
#############

get_state() {

	# Identify the given state
	case "$1" in
	    num)
		state="Num Lock"
		;;

	    scroll)
		state="Scroll Lock"
		;;

	    *)
		state="Caps Lock"
		;;
	esac

	# return the state
	xset q | grep -Po "$state"':\K[^\d]+' | tr -d ' '
}

get_layout() {
	setxkbmap -v | awk -F "+" '/symbols/ {print $2}' | tr '[:lower:]' '[:upper:]'
}

########
# Main #
########

main() {

	caps_state=$(get_state caps | grep -q on && echo "C")
	num_state=$(get_state num | grep -q on && echo "N")
	scroll_state=$(get_state scroll | grep -q on && echo "S")

	# Array with all states
	all_states=( "$num_state" "$caps_state" "$scroll_state" )

	# Get the keyboard layout now
	key_layout="$(get_layout)"

	# Inside data initialization
	inside_data=""

	# Iterate array
	for each in "${all_states[@]}"; do
		if [ -n "$each" ]; then
			inside_data="${inside_data}${each}"
		else
			inside_data="${inside_data} "
		fi
	done

	# Check if it is empty and exit
	if [ -n "${inside_data// }" ]; then
		new_inside=" - ${inside_data}"
	fi

	# Build whole block prettily
	folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"
	source "${folder_now}/../visual/xmobar-style.bash"

	# Print block
	build_block "" " ${key_layout}${new_inside} " "  "

}

main
