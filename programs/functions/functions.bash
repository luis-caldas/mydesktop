#!/usr/bin/env bash

# Globals
ATTACHED="eDP1"
MONITOR="DP2-1"

# All local functions that will be available to the user terminal

function will {
	while true; do
		"${@}" && break
		sleep 1
	done
}

function memdisk {
	watch -d -c -n1 grep -A1 Dirty /proc/meminfo
}

function dock {

	# get the folder in which the script is located
    SOURCE="${BASH_SOURCE[0]}"

    # resolve $SOURCE until the file is no longer a symlink
    while [ -h "$SOURCE" ]; do
      DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
      SOURCE="$(readlink "$SOURCE")"
      [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
    done

    # the final assignment of the directory
    folder_now="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

	# import all modelines
	while read modeline; do
		# read line into array
        read -ra array_modes <<< "${modeline}"
		# clean array
		array_modes[1]="$(tr -d '"' <<< "${array_modes[1]}")"
		# add modes
		xrandr --newmode "${array_modes[@]:1}"
		# add to specific monitor
		xrandr --addmode "${MONITOR}" "${array_modes[1]}"
	done < "${folder_now}/../../displays/nec-v72.modelines"

	# select the best resolution
	xrandr --output "${MONITOR}" --mode "1920x1440_46.00" --primary --right-of "${ATTACHED}" --output "${ATTACHED}" --auto

	# fix rest of stuff for desktop
	xinput map-to-output "Raydium Corporation Raydium Touch System" "${ATTACHED}"
	numlockx on
	reneorevbackslash
	neotrogen restore

}

function dock_legacy {
	xrandr --output "${MONITOR}" --auto --primary --right-of "${ATTACHED}" --output "${ATTACHED}" --auto
	xinput map-to-output "Raydium Corporation Raydium Touch System" "${ATTACHED}"
	numlockx on
	reneorevbackslash
	neotrogen restore
}

function after {
	xinput map-to-output "Raydium Corporation Raydium Touch System" "${ATTACHED}"
	neotrogen restore
}

function wher {
	readlink "$(whereis "${1}" | awk '{print $2}')"
}
