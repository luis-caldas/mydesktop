#!/usr/bin/env bash

function main() {

    authority_file="${HOME}""/.config/xorg/XAuthority"
    init_file="${HOME}""/.xinitrc"

    tty=$(tty)
    if expr "$tty" : '/dev/tty[0-9][0-9]*$' > /dev/null; then
        tty_num=$(echo "$tty" | grep -oE '[0-9]+$')
        vtarg="vt$tty_num -keeptty"
    fi

    echo "$vtarg"

    exec xinit "$init_file" -- "$vtarg" -auth "$authority_file" -logfile "$(mktemp)" 2>&1 | logger -t "neox"

}

main "$@"

