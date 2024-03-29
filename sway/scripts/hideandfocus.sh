#!/usr/bin/bash

cur_focus="$(swaymsg -t get_tree | jq -r '.. | select(.type?) | select(.focused==true) | .app_id')"

if [ "$cur_focus" == "floatterm" ] || [ "$cur_focus" == "music" ] || [ "$cur_focus" == "files" ]; then
    swaymsg scratchpad show
fi
if [[ "$1" == "secrets" ]]; then
    wlrctl window focus "org.gnome.World.Secrets" || swaymsg exec "$1"

elif [[ "$1" == "footws2" ]]; then
    wlrctl window focus "footws2" || swaymsg exec "foot -a footws2"
    
elif [[ "$1" == "emacs" ]]; then
    wlrctl window focus "emacs" || swaymsg exec "emacsclient -r -a emacs"

else
    wlrctl window focus "$1" || swaymsg exec "$1"
fi
