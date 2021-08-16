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

########
# Main #
########

main() {

	caps_state=$(get_state caps | grep -q on && echo "CAPS")
	num_state=$(get_state num | grep -q on && echo "NUM")
	scroll_state=$(get_state scroll | grep -q on && echo "SCROLL")

	# Array with all states
	all_states=( "$caps_state" "$num_state" "$scroll_state" )

	# Inside data initialization
	inside_data=""

	# Iterate array
	for each in "${all_states[@]}"; do
		if [ -n "$each" ]; then
			inside_data="${inside_data}${each} "
		fi
	done

	# Check if it is empty and exit
	if [ -z "$inside_data" ]; then
		exit 1
	fi

	# Trim string and add brackets if it is not empty
	printf "[%s] \n" "$(xargs <<< "$inside_data")"

}

main
