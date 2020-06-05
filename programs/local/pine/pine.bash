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

# Covers the string with given chars
cover() {
    cleft="["
    cright="]"
    printf "%c%s%c" "$cleft" "$*" "$cright"
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

backlight() {
    # Check if light throws an error (no backlight found)
    if ! { light 2>&1 >&3 3>&- | grep '^' >&2; } &>/dev/null ; then
        light=$(light)
        printf "%3.0f" "$light"
    else
        printf " --"
    fi
}

power() {
    cover "$(bat_capacity)" "$(bat_time)" "$(bat_charging)"
}
clight() {
    cover "$(backlight)"
}
all() {
    printf "%s %s \n" "$(power)" "$(clight)"
}
all_pine() {
    if [ -e "$BATTERY_FILE" ]; then
        all
    fi
}

usage() {
    echo "Usage: $0 {capacity,time,charging,light,power,backlight,all,pine}"
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
    light)
        backlight
        ;;
    power)
        power
        ;;
    backlight)
        clight
        ;;
    all)
        all
        ;;
    pine)
        all_pine
        ;;
    -h|--help)
        usage
        exit 64
        ;;
    *)
        usage
        exit 1
        ;;
esac
