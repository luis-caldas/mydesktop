#!/usr/bin/env bash

# Set airport
AIRPORT="EICK"

# Launches a st with given size and the weather command inside
st \
	-T "wneoweather" \
	-g 150x43 \
	-f mono:size=12 \
	-e sh \
	-c 'curl wttr.in/'"${AIRPORT}"'?3 -s | head -n-3 && read'
