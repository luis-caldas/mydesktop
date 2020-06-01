#!/usr/bin/env bash

BATTERY_PATH="/sys/class/power_supply/cw2015-battery"

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
        echo " ---"
    else
        xargs printf "%4s" < "$time_file"
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
        exit 0
        ;;
    *)
        usage
        exit 1
        ;;
esac
