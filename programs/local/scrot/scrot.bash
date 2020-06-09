#!/usr/bin/env bash

SCREENSHOT_PATH="${HOME}/pics/scrots"
PIC_EXTENSION=".png"
NAMO=$(basename "${0}")

# {{{ Utils

foldrizer() {
    if [ ! -d "${SCREENSHOT_PATH}" ]; then
        mkdir -p "${SCREENSHOT_PATH}"
    fi
}

stamp() {
    date +"%Y-%m-%d-%H%M%S_${NAMO}"
}

# }}}

save() {
    # Create scrot folder
    foldrizer
    # Get scrot name
    scrot_name=$(stamp)
    # Save screen shot
    scrot "${1}" -e 'mv $f '"${SCREENSHOT_PATH}"
}

usage() {
    echo "${0}" "{s|select|-h|--help}"
}

extra_lags=""
case $1 in
    s|select)
        extra_flags="-s"
        sleep 1
        ;;
    -h|-help)
        usage
        exit 64
        ;;
    *)
        extra_flags="-p"
        ;;
esac

save "$extra_flags"
