#!/usr/bin/env bash

###########
# Globals #
###########

MIN_PERCENT=30
MAX_PERCENT=255

STORE_BACKLIGHT="$HOME""/.config/my_backlight"

##########
# Script #
##########

function main() {

    # Extract the mode that was given in args
    mode=$(echo "$1" | tr '[:lower:]' '[:upper:]')

    # Get the first path that appears when finding the backlights
    brightness_path=$(find /sys/class/backlight/* | head -n1)"/brightness"

    # Get the value that is set now as backlight
    value_now=$(cat "$brightness_path")

    # Check if we should add or remove the brightness
    if [ "$mode" = "I" ]; then
       new_value=$(( value_now + $2 ))
       if [ "$new_value" -le "$MAX_PERCENT" ]; then
           echo "$new_value" > "$brightness_path"
           echo "$new_value"
       else
           echo "$MAX_PERCENT" > "$brightness_path"
           echo "$MAX_PERCENT"
       fi
   elif [ "$mode" = "D" ]; then
       new_value=$(( value_now - $2 ))
       if [ "$new_value" -gt "$MIN_PERCENT" ]; then
           echo "$new_value" > "$brightness_path"
           echo "$new_value"
       else
           echo "$MIN_PERCENT" > "$brightness_path"
           echo "$MIN_PERCENT"
       fi
    elif [ "$mode" = "C" ]; then
        if [ ! -f "$STORE_BACKLIGHT" ]; then
            store_back="$MAX_PERCENT"
        else
            store_back=$(cat "$STORE_BACKLIGHT")
        fi    
        if [ "$value_now" -le "$MIN_PERCENT" ]; then
            echo -n "0" > "$STORE_BACKLIGHT"
            echo "$store_back" > "$brightness_path"
            echo "$store_back"
        else
            echo -n "$value_now" > "$STORE_BACKLIGHT"
            echo "0" > "$brightness_path"
            echo "0"
        fi
    else
        percentage_now=$(( value_now * 100 / 255 ))
        echo "$percentage_now"
    fi

}

main "$@"
