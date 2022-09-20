#!/usr/bin/env bash

POPUP_ID="30082"

# Get our folder
folder_now="$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")"

# Get volume from a given sink
function get_sink_vol() {
	pamixer --get-volume
}

# Get muted state from a given sink
function get_sink_muted() {
	pamixer --get-mute
}

# Create icon name
function gen_icon_name () {
	"${folder_now}/generate-icon.bash" "audio-volume-${1}.svg"
}

# Create the notification function
function notfy() {
	dunstify -i "$(gen_icon_name "${1}")" "${2}" "\n${3}" -h "int:value:${4}" -r "${POPUP_ID}" -t 1500
}

function main() {

	# Get default sink
	default_sink=$(get_sink)

	# Get volume and muted state
	vol_sink=$(get_sink_vol)
	muted_sink=$(get_sink_muted)

	# Check if it is muted
	if [ "$muted_sink" == "true" ]; then
		notfy "muted" "Volume" "Muted - ${vol_sink} %" "${vol_sink}"
	else
		# Overflow limit
		[ "$vol_sink" -lt 0 ] && vol_sink=0

		# Get correct icon
		icon_setted="low"
		if [ "$vol_sink" -gt 100 ]; then
			icon_setted="overamplified"
		elif [ "$vol_sink" -gt 0 ]; then
			icons=( "${icon_setted}" "medium" "high" )
			# Get correct icon for volue
			icon_ratio=$(awk -v n="${vol_sink}" -v m="${#icons[@]}" 'BEGIN{print int( m * ( n / 100 ) )}')
			# Catch bigger ratios
			[ "$icon_ratio" -ge 3 ] && icon_ratio=2
			# Get proper icon name
			icon_setted="${icons[icon_ratio]}"
		fi

		# Send the notification
		notfy "${icon_setted}" "Volume" "${vol_sink} %" "${vol_sink}"

	fi

}

main "$@"
