#!/usr/bin/env bash

temper_get() {
	temper_value="$(temper | awk '{print ($7+0)}')"
	[ -n "''${temper_value}" ] || return 1
	return 0
}

temper_check() {
	temper_get || return 1
	return 0
}

temper_create() {
	cat <<- EOF
		CHART temper.temperature "" "Temperature TEMPer Probe" "Temperature in C"
		DIMENSION temperature Temperature
	EOF
	return 0
}

temper_update() {
	temper_get || return 1
	# write the result of the work.
	cat <<- VALUESEOF
		BEGIN temper.temperature $1
		SET temperature = $temper_value
		END
	VALUESEOF
	return 0
}