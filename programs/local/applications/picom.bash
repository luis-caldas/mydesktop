#!/usr/bin/env bash

DEFAULT_BORDER=10

#############
# Functions #
#############

function extract_scaling_factor() {

	# Run the gsettings command to get the scaling factor
	factor=$(gsettings get org.gnome.desktop.interface scaling-factor 2>/dev/null || false)

	# If the variable is not empty, extract its value, if not assume the scaling is one
	if [ -n "$factor" ]; then
	factor=$(echo "$factor" | awk '{print $NF}')
	fi

	# Check if a few sys vars exist
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$GDK_SCALE"
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$ELM_SCALE"
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$QT_AUTO_SCREEN_SCALE_FACTOR"

	# If nothing was set until now
	# Set it to one
	[ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="1"

	# Return the scaling factor
	echo -n "$factor"

}

########
# Main #
########

# Get the gnomes scaling factor if possible
scaling_factor=$(extract_scaling_factor)

# Calculate the width and dpi with the scaling factor
new_float_border=$(echo "$scaling_factor""*""$DEFAULT_BORDER" | bc)
new_border=${new_float_border%.*}

# Create the conf for picom
read -r -d '' conf_file << EOF
wintypes:
{
  dock = { shadow = false; }
};
EOF

# Run picom with custom settings
picom \
	--conf <(cat <<< "${conf_file}") \
	--shadow \
	--shadow-opacity 0.5 \
	--shadow-radius "$new_border" \
	--shadow-offset-x "$new_border" \
	--shadow-offset-y "$new_border" \
	--shadow-exclude "bounding_shaped" \
	--shadow-exclude "i:e:Conky" \
	--fading \
	--fade-delta 5
