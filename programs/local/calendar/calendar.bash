#!/usr/bin/env bash

# Launches a st with given size and the cal command inside
st -T "neocalendar" -f mono:size=12 -e sh -c 'cal -3 && read' -g "8x64"
