#!/usr/bin/bash

cur_focus="$(swaymsg -t get_tree | jq -r '.. | select(.type?) | select(.focused==true) | .app_id')"

if [ "$cur_focus" == "floatterm" ] || [ "$cur_focus" == "music" ] || [ "$cur_focus" == "files" ]; then
    swaymsg scratchpad show
fi
if [[ "$1" == "keepassxc" ]]; then
    wlrctl window focus "org.keepassxc.KeePassXC" || swaymsg exec "$1"
    sleep 0.5
    until swaymsg -t get_tree | grep "org.keepassxc.KeePassXC"
    do
        sleep 0.3
    done
    setsid -f ~/.config/sway/scripts/waitforkeepass.sh

elif [[ "$1" == "footws2" ]]; then
    wlrctl window focus "footws2" || swaymsg exec "foot -a footws2"
elif [[ "$1" == "krusader" ]]; then
    wlrctl window focus "org.kde.krusader" || swaymsg exec "krusader"
else
    wlrctl window focus "$1" || swaymsg exec "$1"
fi
