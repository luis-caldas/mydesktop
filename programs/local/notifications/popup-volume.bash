#!/usr/bin/env bash

POPUP_ID="30082"

# Function to get current folder
function get_folder() {

    # get the folder in which the script is located
    SOURCE="${BASH_SOURCE[0]}"

    # resolve $SOURCE until the file is no longer a symlink
    while [ -h "$SOURCE" ]; do

      DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

      SOURCE="$(readlink "$SOURCE")"

      # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
      [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"

    done

    # the final assignment of the directory
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

    # return the directory
    echo "$DIR"
}

# Get our folder
folder_now="$(get_folder)"

# Get the default sink from pulseaudio
function get_sink() {
	pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

# Get volume from a given sink
function get_sink_vol() {
	pacmd list-sinks |
		awk '/^\s+name: /{indefault = $2 == "<'"$1"'>"}
		     /^\s+volume: / && indefault {print $5; exit}' | tr -d '%'
}

# Get muted state from a given sink
function get_sink_muted() {
	pacmd list-sinks |
		awk '/^\s+name: /{indefault = $2 == "<'"$1"'>"}
		     /^\s+muted: / && indefault {print $2; exit}' | tr -d '%'
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
	vol_sink=$(get_sink_vol "${default_sink}")
	muted_sink=$(get_sink_muted "${default_sink}")

	# Check if it is muted
	if [ "$muted_sink" == "yes" ]; then
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
