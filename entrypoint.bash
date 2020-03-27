#!/bin/bash

# This file is the entrypoint to this desktops init system,
# this file should be called from your xinit or whatever your system calls
# after xorg initializes

LOG_PATH="/var/log/my_dekstop_init.log"
LOG_PATH_ALTERNATIVE="/tmp/my_desktop_init.log"

function get_folder() {

    # get the folder in which the script is located
    SOURCE="${BASH_SOURCE[0]}"

    # resolve $SOURCE until the file is no longer a symlink
    while [ -h "$SOURCE" ]; do

      DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

      SOURCE="$(readlink "$SOURCE")"

      # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
      [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"

    done

    # the final assignment of the directory
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

    # return the directory
    echo "$DIR"
}

# get current folder
folder_now=$(get_folder)

# export the project folder
ENTRYPOINT_FOLDER="$folder_now"
export ENTRYPOINT_FOLDER

# show if some variable was not found
set -u

# check which path we should use for logging
if [ ! -w "$LOG_PATH" ]; then
    LOG_PATH="$LOG_PATH_ALTERNATIVE"
fi

# call init and log it
bash "$folder_now""/init/xinitrc" &> "$LOG_PATH"
