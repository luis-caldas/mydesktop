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
    cd "${HOME}" || exit 1

    # execute xinit with the new files
    exec xinit "$init_file" -- "$vtarg" -auth "$authority_file" -logfile "/dev/null" 2>&1 | logger -t "neox"

}

main "$@"

