#!/usr/bin/env bash

# Acquire all networks
networks="$(nmcli c show --active | sed '1d' | awk '{ print $(NF - 0) }' | xargs echo)"

# Print all networks if not empty, else print none
if [ -n "$networks" ]; then
	echo -n "$networks"
else
	echo -n none
fi
