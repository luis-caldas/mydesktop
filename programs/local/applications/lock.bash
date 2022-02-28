#!/usr/bin/env bash

TIMEOUT=10 # seconds

# blank screen
xset dpms force off

# set dpms timeout to someting fast
xset dpms "$TIMEOUT" "$TIMEOUT" "$TIMEOUT"

# lock screen
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

# restore the timeout to nothing
xset dpms 0 0 0
