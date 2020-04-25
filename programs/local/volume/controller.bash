#!/usr/bin/env bash

# Helper for setting the pulseaudio volume from the shell

###########
# Globals #
###########

DEFAULT_VOLUME_STEP=5

#############
# Functions #
#############

# get the current sink from pulseaudio
get_sink() {
    pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

# gets the sink volume from the name
# receives the sink name as the first argument
get_sink_vol() {
    pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'"$1"'>"}
            /^\s+volume: / && indefault {print $5; exit}' | tr -d '%'
}

# sets the pulseaudios sink
set_sink_vol() {
    pactl set-sink-volume @DEFAULT_SINK@ "$1"
}

# toggles the mute state of the defaults pulseaudio sink
mute_sink() {
    pactl set-sink-mute @DEFAULT_SINK@ toggle
}

########
# Main #
########

case "$1" in
    show)
        sink_name=$(get_sink)
        get_sink_vol "$sink_name"
        ;;

    raise)
        set_sink_vol +"$DEFAULT_VOLUME_STEP"%
        ;;

    lower)
        set_sink_vol -"$DEFAULT_VOLUME_STEP"%
        ;;

    mute)
        mute_sink
        ;;

    *)
        echo "Usage: $0 {show|raise|lower|mute}"
        ;;
esac
