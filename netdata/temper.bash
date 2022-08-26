#!/usr/bin/env bash

VEND_ID="1a86"
PROD_ID="e025"

temper_update_every=1

temper_script() {

	# Find all devices that match the IDs
	hids_list=()
	for each_file in /dev/hidraw*; do
		file_name=${each_file##*/}
		dev_id="$(grep HID_ID < "/sys/class/hidraw/${file_name}/device/uevent" | cut -d '=' -f2 | tr '[:upper:]' '[:lower:]')"
		vend_id_now="$(echo "$dev_id" | cut -d ':' -f2)"
		prod_id_now="$(echo "$dev_id" | cut -d ':' -f3)"
		if [ "${vend_id_now: -4}" == "$VEND_ID" ] && [ "${prod_id_now: -4}" == "$PROD_ID" ] && [ -r "$each_file" ]; then
			hids_list+=("$each_file")
		fi
	done

	# Check if we found any
	if [ ${#hids_list[@]} -eq 0 ]; then
		return 1
	fi

	# Attach found hid to fd 5
	exec {hid_fd}<> "${hids_list[-1]}"

	# Send data query
	echo -e '\x00\x01\x80\x33\x01\x00\x00\x00\x00\c' >&$hid_fd

	# Get binary response for the two temps
	int_bin="$(dd count=1 bs=8 <&${hid_fd} 2>/dev/null | xxd -p)"
	ext_bin="$(dd count=1 bs=8 <&${hid_fd} 2>/dev/null | xxd -p)"

	# Extract the values from the hex
	int_val="$((16#${int_bin:4:4}))"
	ext_val="$((16#${ext_bin:4:4}))"

	echo "$int_val" "$ext_val"
	return 0

}

temper_get() {
	script_return_data="$(temper_script)"
	script_return_value="$?"
	read -ra temper_values < <(echo "$script_return_data")
	return "$script_return_value"
}

temper_check() {
	temper_get || return 1
	return 0
}

temper_create() {
	cat <<- EOF
		CHART temper.temperature "" "Temperature TEMPer Probe" "Temperature in C"
		DIMENSION int_temp "Device Temperature" absolute 1 100
		DIMENSION ext_temp "Probe Temperature" absolute 1 100
	EOF
	return 0
}

temper_update() {
	temper_get || return 1
	# write the result of the work.
	cat <<- VALUESEOF
		BEGIN temper.temperature $1
		SET int_temp = ${temper_values[0]}
		SET ext_temp = ${temper_values[1]}
		END
	VALUESEOF
	return 0
}