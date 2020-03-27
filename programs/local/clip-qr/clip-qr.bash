#!/bin/bash

QR_CODE_PIXEL_SIZE=6  # pixel size scaling in pixels

#############

main () {
    # save clip data to variable
    clip_data=$(xclip -selection c -o)

    # create temp file and store its path
    temp_file_path=$(mktemp)

    # generate the qr code into the file
    qrencode -s "$QR_CODE_PIXEL_SIZE" -o "$temp_file_path" "$clip_data"

    # show the file
    xdg-open "$temp_file_path"

    exit 0
}

main "$@"
