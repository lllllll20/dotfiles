#!/usr/bin/bash

if swaymsg -t get_tree | grep 'floatterm';
then
    cur_focus="$(swaymsg -t get_tree | jq -r '.. | select(.type?) | select(.focused==true) | .app_id')"

    if [ "$cur_focus" == "floatterm" ]; then
        swaymsg scratchpad show
    elif [ "$cur_focus" == "files" ] || [ "$cur_focus" == "music" ]; then
        swaymsg scratchpad show
        swaymsg [app_id="floatterm"] focus
    else
        swaymsg [app_id="floatterm"] focus
    fi
else
    foot -a floatterm
fi
