#!/usr/bin/env bash

# Shows lock keys status

#############
# Functions #
#############

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

	caps_state=$(get_state caps | grep -q on && echo "C")
	num_state=$(get_state num | grep -q on && echo "N")
	scroll_state=$(get_state scroll | grep -q on && echo "S")

	# Array with all states
	all_states=( "$num_state" "$caps_state" "$scroll_state" )

	# Inside data initialization
	inside_data=""

	# Iterate array
	for each in "${all_states[@]}"; do
		inside_data+=" "
		if [ -n "$each" ]; then
			inside_data="${each}"
		fi
	done

	# Check if it is empty and exit
	if [ -z "${inside_data// }" ]; then
		exit 1
	fi

	# Build whole block prettily
	folder_now="$(get_folder)"
	source "${folder_now}/../xmobar/style.bash"

	# Print block
	build_block "" "${inside_data}" ""

}

main
