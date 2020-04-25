#!/usr/bin/env bash

# Helper for setting the pulseaudio volume from the shell

###########
# Globals #
###########

STRING_VOLUME="- Volume -"
STRING_BACKLIGHT="- Backlight -"

########
# Main #
########

# acquire the local programs folder
programs_folder="$LOCAL_PROGRAMS_PATH"

case "$1" in
    volume-up)
        bash "$programs_folder""/volume/controller.bash" raise
        volume=$(bash "$programs_folder""/volume/controller.bash" show)
        bash "$programs_folder""/osd/osd.bash" "$volume" "$STRING_VOLUME"
        ;;

    volume-down)
        bash "$programs_folder""/volume/controller.bash" lower
        volume=$(bash "$programs_folder""/volume/controller.bash" show)
        bash "$programs_folder""/osd/osd.bash" "$volume" "$STRING_VOLUME"
        ;;

    volume-mute)
        bash "$programs_folder""/volume/controller.bash" mute
        volume=$(bash "$programs_folder""/volume/controller.bash" show)
        bash "$programs_folder""/osd/osd.bash" "$volume" "$STRING_VOLUME"
        ;;

    backlight-up)
        bash "$programs_folder""/backlight/change.bash" i 10
        brightness=$(bash "$programs_folder""/backlight/change.bash")
        bash "$programs_folder""/osd/osd.bash" "$brightness" "$STRING_BACKLIGHT"
        ;;

    backlight-down)
        bash "$programs_folder""/backlight/change.bash" d 10
        brightness=$(bash "$programs_folder""/backlight/change.bash")
        bash "$programs_folder""/osd/osd.bash" "$brightness" "$STRING_BACKLIGHT"
        ;;

    rofi)
        bash "$programs_folder""/rofi/rofi.bash"
        ;;

    clip-qr)
        bash "$programs_folder""/clip-qr/clip-qr.bash"
        ;;

    # run exerything that is sent after the command argument
    command)
        exec -- "${@:2}"
        ;;

    *)
        echo "Usage: $0 {volume-up|volume-down|volume-mute|backlight-up|backlight-down|rofi|command}"
        ;;
esac
