#!/usr/bin/bash

if swaymsg -t get_tree | grep 'music';
then
    cur_focus="$(swaymsg -t get_tree | jq -r '.. | select(.type?) | select(.focused==true) | .app_id')"

    if [ "$cur_focus" == "music" ]; then
        swaymsg scratchpad show
    elif [ "$cur_focus" == "floatterm" ] || [ "$cur_focus" == "files" ]; then
        swaymsg scratchpad show
        swaymsg [app_id="music"] focus
    else
        swaymsg [app_id="music"] focus
    fi
else
    foot -a music ncmpcpp
fi
