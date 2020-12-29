#!/usr/bin/env bash

# {{{ Globals

BAT_FULL_SYMBOL="#"

BAT_COLOUR_PERCENTAGE_YELLOW=50
BAT_COLOUR_PERCENTAGE_RED=35

BAT_HEIGHT=10
BAT_WIDTH=21

COLOUR_RED="$(tput setaf 1)"
COLOUR_GREEN="$(tput setaf 2)"
COLOUR_YELLOW="$(tput setaf 3)"
COLOUR_RESET="$(tput sgr0)"

# }}}
# {{{ Functions

multiply_line() {

	for _ in $(seq 1 "$2"); do 
		printf "%s" "$1"
	done

	printf "\n"

}

show_message() {

	batt_percentage="$1"

	# Conform the percentage to be between 0 and 100
	if (( batt_percentage > 100 )); then
		batt_percentage=100;
	fi
	if (( batt_percentage < 0 )); then
		batt_percentage=0;
	fi

	# Get battery colour
	batt_colour="$COLOUR_GREEN"

	# Check input percentage for battery colour
	if (( batt_percentage < BAT_COLOUR_PERCENTAGE_RED )); then
		batt_colour="$COLOUR_RED"
	elif (( batt_percentage < BAT_COLOUR_PERCENTAGE_YELLOW )); then
		batt_colour="$COLOUR_YELLOW"
	fi

	# Calculate battery tip size
	tip_size=$(( BAT_WIDTH / 3 ))

	# Cache the battery simbols
	tip_line=" ""$(multiply_line " " "$tip_size")""$(multiply_line "$BAT_FULL_SYMBOL" "$tip_size")""$(multiply_line " " "$tip_size")"" "
	border_line="$BAT_FULL_SYMBOL""$(multiply_line "$BAT_FULL_SYMBOL" "$BAT_WIDTH")""$BAT_FULL_SYMBOL"
	empty_line="$BAT_FULL_SYMBOL""$batt_colour""$(multiply_line " " "$BAT_WIDTH")""$COLOUR_RESET""$BAT_FULL_SYMBOL"
	full_line="$BAT_FULL_SYMBOL""$batt_colour""$(multiply_line "$BAT_FULL_SYMBOL" "$BAT_WIDTH")""$COLOUR_RESET""$BAT_FULL_SYMBOL"

	# Calculate the full and empty lines of the drawing
	full_bar_nr=$(( batt_percentage / BAT_HEIGHT ))
	empty_bar_nr=$(( BAT_HEIGHT - full_bar_nr ))

	# Start drawing the battery
	echo "$tip_line"
	echo "$border_line"

	for _ in $(seq 1 "$empty_bar_nr"); do
		echo "$empty_line"
	done

	for _ in $(seq 1 "$full_bar_nr"); do
		echo "$full_line"
	done

	echo "$border_line"

	# Spacing for new comment
	printf "\n\n\n\n\n\n\n"

	# Show percentage
	figlet "$batt_percentage""%"

}

column_message() {
	show_message "$1" | pr -2 -t -i" 1"
}

# }}}
# {{{ Main

usage() {
	echo "Usage: $0 {battery-percentage}"
}

case "$1" in
	''|*[!0-9]*)
		usage
		;;
	*)
		column_message "$1"
		;;
esac

# }}}

