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
