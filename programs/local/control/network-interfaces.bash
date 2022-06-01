#!/usr/bin/env bash

# Acquire all networks
networks="$(nmcli c show --active | sed '1d' | awk '{ print $(NF - 0) }')"

# Print all networks if not empty, else print none
if [ -n "$networks" ]; then
	wc -l <<< "${networks}"
else
	echo -n 0
fi
