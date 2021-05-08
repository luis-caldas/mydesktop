#!/usr/bin/env bash

# Launches a st with given size and the cal command inside
st \
	-T "neosysinfo" \
	-g 100x30 \
	-f mono:size=12 \
	-e sh \
	-c 'gotop'
