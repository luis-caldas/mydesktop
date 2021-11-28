#!/usr/bin/env bash

# put here all the functions you want to be sourced at init
function will {
	while true; do
		"${@}" && break
		sleep 1
	done
}

function memdisk {
	watch -d -c -n1 grep -A1 Dirty /proc/meminfo
}

function lap_dock {
	xrandr --output DP2-1 --auto --primary --right-of eDP1 --output eDP1 --auto
	xinput map-to-output "Raydium Corporation Raydium Touch System" eDP1
	swap-slash-and-back
	neotrogen restore
}

function after_monitor_lap {
	xinput map-to-output "Raydium Corporation Raydium Touch System" eDP1
	neotrogen restore
}
