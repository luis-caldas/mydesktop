#!/bin/bash

########
# Vars #
########

DEFAULT_WIDTH=750
DEFAULT_DPI=100
DEFAULT_FONT="mono 10"

#############
# Functions #
#############

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

########
# Main #
########

# get the gnomes scaling factor if possible
scaling_factor=$(extract_scaling_factor)

# calculate the width and dpi with the scaling factor
new_float_width=$(echo "$scaling_factor""*""$DEFAULT_WIDTH" | bc)
new_float_dpi=$(echo "$scaling_factor""*""$DEFAULT_DPI" | bc)
new_width=${new_float_width%.*}
new_dpi=${new_float_dpi%.*}

# run rofi with our configs
rofi -show drun -modi drun -location 0 -padding 10 -width "$new_width" \
        -lines 10 -line-margin 2 -line-padding 2 \
        -separator-style none -font "$DEFAULT_FONT" -columns 1 \
        -bw 1 \
        -dpi "$new_dpi" \
        -no-click-to-exit \
        -disable-history \
        -hide-scrollbar \
        -color-window "#F2F2F2, #000000, #000000" \
        -color-normal "#F2F2F2, #000000, #F2F2F2, #01007F, #F2F2F2" \
        -color-active "#F2F2F2, #000000, #F2F2F2, #007763, #000000" \
        -color-urgent "#F2F2F2, #000000, #F2F2F2, #77003d, #000000" \
        -kb-row-select "Tab" -kb-row-tab ""
