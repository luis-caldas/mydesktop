#!/usr/bin/env bash

# Launches a st with given size and the command inside
st \
	-T "wneonetwork" \
	-g 90x30 \
	-f mono:size=12 \
	-e sh \
	-c 'nmtui'
