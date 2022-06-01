#!/usr/bin/env bash

TIMEOUT=7500

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

# Get info from ifname
function get_info() {
	nmcli device show "${1}"
}

# Get connected wifi from ifname
function get_wifi() {
	nmcli device wifi list ifname "${1}" --rescan no | grep '^\*'
}

# Notifies the wifi
function wifi_not() {

	# Reassing some variables
	line="$(get_wifi "${1}")"

	wssid="$(awk '{print $3}' <<< "${line}")"
	wchan="$(awk '{print $5}' <<< "${line}")"
	wrate="$(awk '{print $6}' <<< "${line}")"
	wunit="$(awk '{print $7}' <<< "${line}")"
	wsign="$(awk '{print $8}' <<< "${line}")"
	wsecu="$(awk '{print $10}' <<< "${line}")"

	wdevc="${1}"
	ipaddr="${2}"
	gateway="${3}"

	# Create the information line
	info_line="$(printf "%s%%\nCh %s - %s %s - %s\n%s @ %s\n(%s)" "${wsign}" "${wchan}" "${wrate}" "${wunit}" "${wsecu}" "${ipaddr}" "${gateway}" "${wdevc}")"

	# Get the appropriate icon
	icon_ratio=$(awk -v n="${wsign}" -v m="${#wifi_icons[@]}" 'BEGIN{print int( m * ( n / 100 ) )}')
	# Conform icon ratio
	[ "$icon_ratio" -ge "${#wifi_icons[@]}" ] && icon_ratio=$(( ${#wifi_icons[@]} - 1 ))

	# Send the notification
	notfy_bar "${wifi_icon_prefix}${wifi_icons[icon_ratio]}" "${wssid}" "${info_line}" "${wsign}"

}

# Create icon name
function gen_icon_name () {
	"${folder_now}/generate-icon.bash" "${1}.svg"
}

# Create the notification function
function notfy() {
	dunstify -i "$(gen_icon_name "${1}")" "${2}" "${3}" -t "${TIMEOUT}"
}
function notfy_bar() {
	dunstify -i "$(gen_icon_name "${1}")" "${2}" "${3}" -h "int:value:${4}" -t "${TIMEOUT}"
}

function main() {

	# Get connected devices
	connected_devices="$(nmcli device | grep '[^A-z]connected[^A-z]')"

	# Array of icons and variables
	wifi_icon_prefix="wireless-"
	wifi_icons=( "none" "weak" "ok" "good" "excellent")
	network_icon="network"

	if [ -z "${connected_devices}" ]; then
		exit
	fi

	# Rearange order of lines
	new_lines="$({
		grep wifi <<< "${connected_devices}" | awk '{ print $1 " " $2 }';
		connected_devices="$(grep -v wifi <<< "${connected_devices}")";
		grep ethernet <<< "${connected_devices}" | awk '{ print $1 " " $2 }';
		connected_devices="$(grep -v ethernet <<< "${connected_devices}")";
		[ -n "${connected_devices}" ] && awk '{ print $1 " " $2 }' <<< "${connected_devices}";
	})"

	# Iterate the connected devices and notify them
	while read -r line; do

		# Extract some specific data
		wdevc="$(awk '{print $1}' <<< "${line}")"
		wtype="$(awk '{print $2}' <<< "${line}")"

		# Get ip and gateway with data got
		shown_data="$(get_info "${wdevc}")"
		wip="$(grep "IP4.ADDRESS" <<< "${shown_data}" | head -n1 | awk '{print $2}')"
		wconn="$(grep "GENERAL.CONNECTION" <<< "${shown_data}" | head -n1 | awk '{print $2}')"
		wtype="$(grep "GENERAL.TYPE" <<< "${shown_data}" | head -n1 | awk '{print $2}')"
		wroute="$(grep "IP4.GATEWAY" <<< "${shown_data}" | head -n1 | awk '{print $2}')"

		# Wifi connections get special treatment
		if [ "${wtype}" == "wifi" ]; then

			wifi_not "${wdevc}" "${wip}" "${wroute}"

		# Eth connections also have special treatment
		elif [ "${wtype}" == "ethernet" ]; then

			# Acquire some eth specific info
			eth_info="$(ethtool "${wdevc}" 2> /dev/null)"

			# Extract wanted information
			eth_speed="$(grep "Speed:" <<< "${eth_info}" | head -n1 | awk '{print $2}')"
			eth_duplex="$(grep "Duplex:" <<< "${eth_info}" | head -n1 | awk '{print $2}')"

			# Create the notification line
			info_line="$(printf "%s\n%s - %s\n%s @ %s\n(%s)" "${wtype}" "${eth_speed}" "${eth_duplex}" "${wip}" "${wroute}" "${wdevc}")"
			notfy "${network_icon}" "${wconn}" "${info_line}"

		# Print simple info for other connections
		else

			# Create the notification line
			info_line="$(printf "%s\n%s @ %s\n(%s)" "${wtype}" "${wip}" "${wroute}" "${wdevc}")"
			notfy "${network_icon}" "${wconn}" "${info_line}"

		fi


	done <<< "${new_lines}"

}

main "$@"
