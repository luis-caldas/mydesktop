#!/usr/bin/env bash

function main() {

	# Variable reasignment
	newScale="$1"

	# Set proper files
	authority_file="${HOME}""/.config/xorg/XAuthority"
	init_file="${HOME}""/.xinitrc"

	# Get the current tty
	tty=$(tty)
	if expr "$tty" : '/dev/tty[0-9][0-9]*$' > /dev/null; then
		tty_num=$(echo "$tty" | grep -oE '[0-9]+$')
		vtarg="vt$tty_num -keeptty"
	fi

	# Change the working directory to the users home folder
	cd "${HOME}" || exit 1

	# Export variable if it exists
	if [ -n "$newScale" ]; then
		systemctl --user set-environment NEW_SCALE="$newScale"
	fi

	# Execute xinit with the new files
	exec xinit "$init_file" -- "$vtarg" -auth "$authority_file" -logfile "/dev/null" 2>&1 | logger -t "neox"

}

main "$@"

