#!/bin/bash

# Check if DP-6 is active
if swaymsg -t get_outputs -r | jq '.[] | select(.name == "DP-6") | .active' | grep -q true; then
    swaymsg output DP-6 disable
else
    swaymsg output DP-6 enable
fi
