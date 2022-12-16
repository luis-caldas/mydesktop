#!/usr/bin/env bash

# Main strftime used to start the info
MAIN_STRFTIME_START="%Y/%m/%d %T "
MAIN_STRFTIME_END=" %a %W"

# Create main correlation list of the timezones and military
declare -A MILTIME
MILTIME["+0100"]='A'  # Alfa
MILTIME["+0200"]='B'  # Bravo
MILTIME["+0300"]='C'  # Charlie
MILTIME["+0400"]='D'  # Delta
MILTIME["+0500"]='E'  # Echo
MILTIME["+0600"]='F'  # Foxtrot
MILTIME["+0700"]='G'  # Golf
MILTIME["+0800"]='H'  # Hotel
MILTIME["+0900"]='I'  # India
MILTIME["+1000"]='K'  # Kilo
MILTIME["+1100"]='L'  # Lima
MILTIME["+1200"]='M'  # Mike
MILTIME["-0100"]='N'  # November
MILTIME["-0200"]='O'  # Oscar
MILTIME["-0300"]='P'  # Papa
MILTIME["-0400"]='Q'  # Quebec
MILTIME["-0500"]='R'  # Romeo
MILTIME["-0600"]='S'  # Sierra
MILTIME["-0700"]='T'  # Tango
MILTIME["-0800"]='U'  # Uniform
MILTIME["-0900"]='V'  # Victor
MILTIME["-1000"]='W'  # Whiskey
MILTIME["-1100"]='X'  # X-ray
MILTIME["-1200"]='Y'  # Yankee
MILTIME["+0000"]='Z'  # Zulu

# Correlation list for the phonetic alphabet
declare -A PHONETIC
PHONETIC['A']="Alfa"
PHONETIC['B']="Bravo"
PHONETIC['C']="Charlie"
PHONETIC['D']="Delta"
PHONETIC['E']="Echo"
PHONETIC['F']="Foxtrot"
PHONETIC['G']="Golf"
PHONETIC['H']="Hotel"
PHONETIC['I']="India"
PHONETIC['K']="Kilo"
PHONETIC['L']="Lima"
PHONETIC['M']="Mike"
PHONETIC['N']="November"
PHONETIC['O']="Oscar"
PHONETIC['P']="Papa"
PHONETIC['Q']="Quebec"
PHONETIC['R']="Romeo"
PHONETIC['S']="Sierra"
PHONETIC['T']="Tango"
PHONETIC['U']="Uniform"
PHONETIC['V']="Victor"
PHONETIC['W']="Whiskey"
PHONETIC['X']="X-ray"
PHONETIC['Y']="Yankee"
PHONETIC['Z']="Zulu"

# Get the selected date first
start_date="$(date +"$MAIN_STRFTIME_START")"
end_date="$(date +"$MAIN_STRFTIME_END")"

# Get the timezone
timezone_now="$(date +"%z")"

# Fix it if possible
fixed_timezone="${MILTIME["$timezone_now"]}"
if [ -z "$fixed_timezone" ]; then
    fixed_timezone='?'
fi

# Print the whole thing capitalised
tr '[:lower:]' '[:upper:]' <<< "${start_date}${fixed_timezone}${end_date}"
