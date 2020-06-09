#!/usr/bin/env bash

NITROGEN_HOME="${HOME}/.config/nitrogen"
PAPE_PATH="${HOME}/.local/share/backgrounds"

if [ ! -f "${NITROGEN_HOME}/bg-saved.cfg" ]; then
    # We hope that the user set a wallpaper
    echo "Nitrogen doesn't have any wallpaper set, using a random one"
    nitrogen --set-zoom-fill --random "$PAPE_PATH"
else
    # Nitrogen already is set
    echo "There is a bg-saved.cfg file, restoring nitrogen"
    nitrogen --restore "$PAPE_PATH"
fi
