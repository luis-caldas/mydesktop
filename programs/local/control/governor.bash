#!/usr/bin/env bash

# {{{ Config

# Folders
GPU_FOLDER="/sys/class/devfreq"
CPU_FOLDER="/sys/devices/system/cpu"
CPU_FOLDER_AFTER="/cpufreq"

# Names
GPU_NAMES=( "ff9a0000.gpu" )
POSSIBLE_LIST_NAMES=( available_governors scaling_available_governors )
POSSIBLE_CHANGE_NAMES=( governor scaling_governor )

# Config folders
SAVE_FOLDER="${HOME}/.config/neogovernor"

# }}}
# {{{ Utils

good_list() {
	tr ' ' '\n' <<< "$@"
}

check_create_folder() {
	if [ ! -d "$1" ]; then
		mkdir -p "$1"
	fi
}

save_it() {
	echo "${2}" > "${SAVE_FOLDER}/${1}"
}

get_it() {
	cat "${SAVE_FOLDER}/${1}"
}

# }}}
# {{{ Functions
# {{{ CPU

cpu_available_governors() {

	# Governor comparing logic
	last_governors=""
	viable_governors=""

	# Iterate all the cpus
	for each_cpu in "${CPU_FOLDER}/cpu"*"${CPU_FOLDER_AFTER}"; do

		# Iterate possible file names
		for each_name in "${POSSIBLE_LIST_NAMES[@]}"; do

			# Save name of the governor file
			gov_file="${each_cpu}/${each_name}"

			# Check if this specific governor file exists
			if [ -f "${gov_file}" ]; then

				# Translate governor file to line separated
				file_content="$(cat "$gov_file")"
				translated_gov="$(good_list "$file_content")"

				# Check if first run and populate last var
				if [ -z "$last_governors" ]; then

					# Assign variables for the first time
					last_governors="$translated_gov"
					viable_governors="$translated_gov"

				else

					# Sort the variables for the compare command
					s_gov="$(echo "$last_governors" | sort)"
					s_tra="$(echo "$translated_gov" | sort)"

					# Get the matching entries
					viable_governors="$(comm -12 <(echo "$s_gov") <(echo "$s_tra"))"

				fi

				# Continue the parent loop for we found a matching file
				continue 2
			fi
		done

		# If we reached here the file was not found
		echo "Could not find the file for the CPU governor in \`${each_cpu}\`"
		exit 1

	done

	# Check if the governors we acquired are valid
	if [ -n "$viable_governors" ]; then
		echo "$viable_governors"
		return
	fi

	# If we reached here there was no governor to be found
	echo "Unable to find a suitable cpu governor"
	exit 1

}

cpu_set_governor() {

	# Possible governors for the cpu
	possible_list="$(cpu_available_governors)"

	# Check if the governor is valid
	if ! echo "$possible_list" | grep -Fxq "${1}"; then
		# No matching governor to one given
		echo "The given governor \`${1}\` is not supported by the gpu"
		exit 1
	fi

	# Iterate all the cpus
	for each_cpu in "${CPU_FOLDER}/cpu"*"${CPU_FOLDER_AFTER}"; do

		# Iterate possible file names
		for each_name in "${POSSIBLE_CHANGE_NAMES[@]}"; do

			# Save name of the governor file
			gov_file="${each_cpu}/${each_name}"

			# If there is a file set it to the governor
			if [ -f "$gov_file" ]; then

				# Set governor
				echo "${1}" > "$gov_file"

				# Save it to the file
				save_it cpu "${1}"

				# Continue parent loop
				continue 2
			fi

		done

		# Error
		echo "Could not find a governor file in \`${each_cpu}\`"
		echo "Some CPUs may be different"
		exit 1

	done

}

cpu_restore_governor() {
	govr="$(get_it cpu)"
	cpu_set_governor "$govr"
}

cpu_get_governor() {

	# Initialize the governor comparing var
	first_governor=""

	# Iterate all the cpus
	for each_cpu in "${CPU_FOLDER}/cpu"*"${CPU_FOLDER_AFTER}"; do

		# Iterate possible file names
		for each_name in "${POSSIBLE_CHANGE_NAMES[@]}"; do

			# Save name of the governor file
			gov_file="${each_cpu}/${each_name}"

			# If there is a file set it to the governor
			if [ -f "$gov_file" ]; then

				# Acquire the governor value now
				gov_now="$(cat "$gov_file")"

				# Set the governor to var if first time
				if [ -z "$first_governor" ]; then

					# Initialize
					first_governor="$gov_now"

				else

					# Compare to see if governors are the
					# same
					if [ "$first_governor" != "$gov_now" ]; then
						
						# Show error
						echo "Some governors on the cpu are different"
						echo "Try setting them again"
						exit 1

					fi

				fi

			fi

		done

	done

	echo "$first_governor"

}

# }}}
# {{{ GPU

gpu_available_device() {

	# Check which gpu exists and chooses the first one
	for each in "${GPU_NAMES[@]}"; do
		if [ -d "${GPU_FOLDER}/${each}" ]; then
			echo "$each"
			return
		fi
	done

	# Error
	echo "No GPU found from the ones given on the list"
	exit 1

}

gpu_available_governors() {

	# Iterate possible files
	for each_file in "${POSSIBLE_LIST_NAMES[@]}"; do
		full_path="${GPU_FOLDER}/${1}/${each_file}"
		if [ -f "${full_path}" ]; then
			good_list "$(cat "$full_path")"
			return
		fi
	done

	# If we reached here there was no governor to be found
	echo "Unable to find a suitable gpu governor"
	exit 1

}

gpu_set_governor() {

	# Get available device
	gpu_device="$(gpu_available_device)"

	# Possible governors for the first found gpu
	possible_list="$(gpu_available_governors "$gpu_device")"

	# Check if the governor is valid
	if ! echo "$possible_list" | grep -Fxq "${1}"; then
		# No matching governor to one given
		echo "The given governor \`${1}\` is not supported by the gpu"
		exit 1
	fi

	# See which governor file the device uses
	for each_file in "${POSSIBLE_CHANGE_NAMES[@]}"; do
		gov_file="${GPU_FOLDER}/${gpu_device}/${each_file}"
		if [ -f "$gov_file" ]; then
			echo "${1}" > "$gov_file"
			save_it gpu "${1}"
			return
		fi
	done

	# Could not find a possible file
	echo "Could not find the GPU governor file"
	exit 1

}

gpu_restore_governor() {
	govr="$(get_it gpu)"
	gpu_set_governor "$govr"
}

gpu_get_governor() {

	# Iterate GPU names
	for each_gpu in "${GPU_NAMES[@]}"; do
		for each_file in "${POSSIBLE_CHANGE_NAMES[@]}"; do
			full_path="${GPU_FOLDER}/${each_gpu}/${each_file}"
			if [ -f "${full_path}" ]; then
				cat "$full_path"
				return;
			fi
		done
	done

	# If we reached here there was no governor to be found
	echo "Unable to find gpu governor"
	exit 1
}

# }}}
# }}}
# {{{ Printing



# }}}
# {{{ Main

usage() {
	echo "Could not understand the command"
}

# First of all create our save folder
check_create_folder "${SAVE_FOLDER}"

case "$1" in
	cpu)
		case "$2" in
			set)
				cpu_set_governor "$3"
				;;
			get)
				cpu_get_governor
				;;
			list)
				cpu_available_governors
				;;
			restore)
				cpu_restore_governor
				;;
			*)
				usage
				exit 1
				;;
		esac
		;;
	gpu)
		case "$2" in
			set)
				gpu_set_governor "$3"
				;;
			get)
				gpu_get_governor
				;;
			list)
				gpu_available_governors "$(gpu_available_device)"
				;;
			restore)
				gpu_restore_governor
				;;
			*)
				usage
				exit 1
				;;
		esac
		;;
	get)
		printf ">>> %s\n" CPU
		cpu_get_governor
		printf ">>> %s\n" GPU
		gpu_get_governor
		;;
	list)
		printf ">>> %s\n" CPU
		cpu_available_governors
		printf ">>> %s\n" GPU
		gpu_available_governors "$(gpu_available_device)"
		;;
	restore)
		cpu_restore_governor
		gpu_restore_governor
		;;
	*)
		usage
		exit 1
		;;
esac

# }}}
