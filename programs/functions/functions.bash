#!/usr/bin/env bash

# put here all the functions you want to be sourced at init
function will {
	while true; do
		"${@}" && break
		sleep 1
	done
}
