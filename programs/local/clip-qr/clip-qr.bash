#!/usr/bin/env bash

QR_CODE_PIXEL_SIZE=6  # pixel size scaling in pixels

function extract_scaling_factor() {

    # run the gsettings command to get the scaling factor
    factor=$(gsettings get org.gnome.desktop.interface scaling-factor 2>/dev/null || false)

    # if the variable is not empty, extract its value, if not assume the scaling is one
    if [ -n "$factor" ]; then
        factor=$(echo "$factor" | awk '{print $NF}')
    fi

    # check if a few sys vars exist
    [ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$GDK_SCALE"
    [ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$ELM_SCALE"
    [ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="$QT_AUTO_SCREEN_SCALE_FACTOR"

    # if nothing was set until now
    # set it to one
    [ -z "$factor" ] || [ "$factor" -lt 1 ] && factor="1"

    # return the scaling factor
    echo -n "$factor"

}

#############

main () {
    # save clip data to variable
    clip_data=$(xclip -selection c -o)

    # create temp file and store its path
    temp_file_path=$(mktemp)

    # calculate the size with scaling
    scaling_factor=$(extract_scaling_factor)
    new_float_size=$(echo "$scaling_factor""*""$QR_CODE_PIXEL_SIZE" | bc)
    new_size=${new_float_size%.*}

    # generate the qr code into the file
    qrencode -s "$new_size" -o "$temp_file_path" "$clip_data"

    # show the file
    if [ -f "$temp_file_path" ]; then
        xdg-open "$temp_file_path"
    fi

    exit 0
}

main "$@"
