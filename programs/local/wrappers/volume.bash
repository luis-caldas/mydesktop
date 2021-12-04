#!/usr/bin/env bash

# Launches a st with given size and the command inside
st \
	-T "wneovolume" \
	-g 150x43\
	-f mono:size=12 \
	-e sh \
	-c 'ncpamixer'
