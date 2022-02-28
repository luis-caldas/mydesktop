#!/usr/bin/env bash

TIMEOUT=10 # seconds

# Check if picom service is running
neopicom_is_running="$(systemctl is-active --user neopicom)"

# Disable picom if it is enabled
[ "${neopicom_is_running}" == "active" ] && systemctl stop --user neopicom

# Blank screen
xset dpms force off

# Set dpms timeout to someting fast
xset dpms "$TIMEOUT" "$TIMEOUT" "$TIMEOUT"

# Lock screen
XSECURELOCK_AUTH_TIMEOUT="$TIMEOUT" \
XSECURELOCK_BLANK_TIMEOUT=5 \
XSECURELOCK_BLANK_DPMS_STATE="off" \
XSECURELOCK_NO_COMPOSITE=1 \
XSECURELOCK_DIM_OVERRIDE_COMPOSITOR_DETECTION=1 \
XSECURELOCK_PASSWORD_PROMPT="time" \
XSECURELOCK_SHOW_USERNAME=0 \
XSECURELOCK_SHOW_HOSTNAME=0 \
XSECURELOCK_AUTH_SOUNDS=1 \
xsecurelock

# Restore the timeout to nothing
xset dpms 0 0 0

# Reenable picom if needed
[ "${neopicom_is_running}" == "active" ] && systemctl start --user neopicom

