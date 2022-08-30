#!/usr/bin/bash

cur_focus="$(swaymsg -t get_tree | jq -r '.. | select(.type?) | select(.focused==true) | .app_id')"

if [ "$cur_focus" == "floatterm" ] || [ "$cur_focus" == "music" ] || [ "$cur_focus" == "files" ]; then
    swaymsg scratchpad show
fi
swaymsg exec "$1"
