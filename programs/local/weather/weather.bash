#!/usr/bin/env bash

# Set airport
AIRPORT="EICK"

# Launches a st with given size and the weather command inside
st \
	-T "neoweather" \
	-g 150x43\
	-f mono:size=8 \
	-e sh \
	-c 'curl wttr.in/'"${AIRPORT}"'?3 -s | head -n-3 && read'
