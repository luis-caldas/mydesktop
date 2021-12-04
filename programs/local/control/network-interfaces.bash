#!/usr/bin/env bash

 nmcli c show --active | sed '1d' | awk '{ print $(NF - 0) }' | xargs echo
