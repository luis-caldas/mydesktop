#!/usr/bin/env bash

NITROGEN_HOME="${HOME}/.config/nitrogen"
PAPE_PATH="${HOME}/.local/share/backgrounds"

# {{{ Utils

count_monitors() {
    xrandr | grep -v 'disconnected' | grep -c 'connected'
}

get_monitors_nitro_file() {
    grep -E "${2}"'[0-9]*' "${1}" | tr -d '[]' | sed -n 's/'"${2}"'//p'
}

start_nitro_for_head() {
    head="${1}"
    nitrogen --set-zoom-fill --head="$head" --random "$PAPE_PATH"
}

# }}}

restore() {
    # get total of monitors
    number_monitors=$(count_monitors)

    # get list with all set monitors
    set_monitors=$(get_monitors_nitro_file "${NITROGEN_HOME}/bg-saved.cfg" "xin_")

    # check if nitrogen config file exists
    if [ -f "${NITROGEN_HOME}/bg-saved.cfg" ]; then
        # restore what we can from the config file
        nitrogen --restore "$PAPE_PATH"
    fi
 
    # iterate all the monitors
    for each_monitor in $(seq 1 "${number_monitors}"); do
        head=$(( each_monitor - 1 )) # fix xinerama offset
        if ! echo "$set_monitors" | grep -qE '^'"$head"'$'; then
            start_nitro_for_head "$head"
        fi
    done
}

run() {
    nitrogen "$PAPE_PATH"
}

usage() {
    echo "${0}" "{restore|-h|--help}"
}

case $1 in
    restore)
        restore
        ;;
    -h|-help)
        usage
        ;;
    *)
        run
        ;;
esac

