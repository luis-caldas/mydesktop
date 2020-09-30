#!/usr/bin/env bash

# Launches a st with given size and the cal command inside
st \
	-T "neocalendar" \
	-g 76x14 \
	-f mono:size=12 \
	-e sh \
	-c 'cal -3 -m --color && read'
