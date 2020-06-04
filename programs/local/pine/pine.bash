#!/usr/bin/env bash

BATTERY_PATH="/sys/class/power_supply/cw2015-battery"

# {{{ Utils

# Transforms minutes to 
mins_to_time() {
    # Time divider
    divider=":"

    # Calculate hours and mins
    ((hours=${1}/60))
    ((minutes=${1}%60))

    # Return the formatted hour
    printf "%2d%c%02d" "$hours" "$divider" "$minutes"
}

# }}}

bat_capacity() {
    capacity_file="$BATTERY_PATH""/capacity"
    if [ ! -e "$capacity_file" ]; then
        echo " --"
    else
        xargs printf "%3s" < "$capacity_file"
    fi
}

bat_time() {
    time_file="$BATTERY_PATH""/time_to_empty_now"
    if [ ! -e "$time_file" ]; then
        echo " ----"
    else
        time_mins=$(cat "$time_file")
        converted_time=$(mins_to_time "$time_mins")
        printf "%5s" "$converted_time"
    fi
}

bat_charging() {
    charge_file="$BATTERY_PATH""/status"
    if [ ! -e "$charge_file" ]; then
        echo " --"
    else
        if grep -Fxq "Charging" "$charge_file"; then
            printf " %s" "/\\"
        else
            printf " %s" "\\/"
        fi
    fi
}

usage() {
    echo "Usage: $0 {battery_capacity,battery_time,battery_charging}"
}

case "$1" in
    capacity)
        bat_capacity
        ;;
    time)
        bat_time
        ;;
    charging)
        bat_charging
        ;;
    -h|--help)
        usage
        ;;
    *)
        usage
        exit 1
        ;;
esac
