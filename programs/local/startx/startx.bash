#!/usr/bin/env bash

function main() {

    # set proper files
    authority_file="${HOME}""/.config/xorg/XAuthority"
    init_file="${HOME}""/.xinitrc"

    # get the current tty
    tty=$(tty)
    if expr "$tty" : '/dev/tty[0-9][0-9]*$' > /dev/null; then
        tty_num=$(echo "$tty" | grep -oE '[0-9]+$')
        vtarg="vt$tty_num -keeptty"
    fi

    # change the working directory to the users home folder
    cd "${HOME}"

    # execute xinit with the new files
    systemd-run --scope --user --unit neox xinit "$init_file" -- "$vtarg" -auth "$authority_file" -logfile "$(mktemp)"

}

main "$@"

