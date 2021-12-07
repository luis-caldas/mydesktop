#!/usr/bin/env bash

# Create some variables used in the bars
XRESOURCE_NAME="xmobar"

left_arrow=""
right_arrow=""

function extract() {
	data=$(xrdb -query | grep "${XRESOURCE_NAME}\.${1}" | cut -f 2)
	[ -z "$data" ] && data="0"
	echo -n "$data"
}

# Function used for styling
colour_arrow() {
	[ "${1}" == "l" ] && arrow="${left_arrow}" || arrow="${right_arrow}"
	echo "<fc=${2}:0><fn=2>${arrow}</fn></fc>"
}

build_block () {

	# Wether we should include an arrow at the start
	[ "${4}" == "e" ] && init_arrow="" || init_arrow="$(colour_arrow r "${colour2},${colour1}")"

	# Do we want actions for this block
	[ -n "${1}" ] && block="<action=\`${1}\`>"

	# Start block with the given icon and create the rest
	block="${block}<fc=${background},${colour1}:0><fn=1> ${3} </fn>${5}</fc>"
	block+="$(colour_arrow r "${colour1},${colour2}")<fc=${foreground},${colour2}:0> ${2} </fc>"

	# If action required add closing tag
	[ -n "${1}" ] && block+="</action>"

	# Finish block with the init arrow if chosen
	block+="${init_arrow}"

	# Echo the whole block
	echo -n "${block}"
}

simple_block() {

	# Do we want actions for this block
	[ -n "${1}" ] && block="<action=\`${1}\`>"

	# Start block with the given icon and create the rest
	block="${block}$(colour_arrow l "${colour0},${colour2}")"
	block+="<fc=${background},${colour0}:0>${2}</fc>"
	block+="$(colour_arrow r "${colour0},${colour2}")"

	# If action required add closing tag
	[ -n "${1}" ] && block+="</action>"

	# Finish block with the init arrow if chosen
	block+="${init_arrow}"

	# Echo the whole block
	echo -n "${block}"

}

# Extract colours xresource
alpha=$(extract "alpha")
background=$(extract "background")
foreground=$(extract "foreground")
colour0=$(extract "colour0")
colour1=$(extract "colour1")
colour2=$(extract "colour2")

# Export local variables
export -- \
	left_arrow right_arrow \
	alpha background foreground \
	colour0 colour1 colour2
