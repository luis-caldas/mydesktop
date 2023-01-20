#!/usr/bin/env bash

# Globals
ATTACHED="eDP"
MONITOR="HDMI-A-0"

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

function rescan {
	nmcli d wifi rescan
}

function mmerge {

	# get current branch
	current_branch="$(git rev-parse --abbrev-ref HEAD)"

	# merge to master
	git switch master
	git merge "${current_branch}"
	git push
	git switch "${current_branch}"
	git status

}

function oneline {
	git log --graph --decorate --oneline
}

function dock {

    # the final assignment of the directory
    folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

	# import all modelines
	while read -r modeline; do
		# read line into array
	    read -ra array_modes <<< "${modeline}"
		# clean array
		array_modes[1]="$(tr -d '"' <<< "${array_modes[1]}")"
		# add modes
		xrandr --newmode "${array_modes[@]:1}"
		# add to specific monitor
		xrandr --addmode "${MONITOR}" "${array_modes[1]}"
	done < "${folder_now}/../../displays/nec-v72.modelines"

	# get the new scaling
	normal_scale="${GDK_SCALE}x${GDK_SCALE}"

	# calculate the xrandr scale
	start=1
	# get boundaries for mirroring value
	range=$(( GDK_SCALE - start ))
	middle="$(bc <<< "scale=2; ${range} / 2")"
	# get pure number of target
	pure="$(bc <<< "${TARGET_SCALE} - ${start}")"
	# calculate inverse correlation
	corr="$(bc <<< "$start + ( $middle + ( $middle - $pure ) )" )"

	# fix the scale
	scaling="${corr}x${corr}"

	# offset for the second monitor
	width="$(xrandr | sed -n "/^${ATTACHED}.*/,/^[^[:space:]]/p" | head -n-1 | awk '/\*/ {print $1}' | head -n1 | cut -d'x' -f1)"
	new_pos=$(( width * GDK_SCALE ))

	# set custom resolution
	custom_res="1440x1080_65.00"

	# select the best resolution
	xrandr --output "${MONITOR}" --mode "${custom_res}" --scale "${scaling}" --primary --pos "${new_pos}"x0 --output "${ATTACHED}" --scale "${normal_scale}" --auto

	# fix rest of stuff for desktop
	cneogovernor cpu set performance
	setxkbmap us
	neotrogen restore

}

function undock {
	xrandr --output "${MONITOR}" --off --output "${ATTACHED}" --auto --primary
	cneogovernor cpu set powersave
	setxkbmap gb
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
