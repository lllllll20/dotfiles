#!/usr/bin/bash

#if [[ ${HOSTNAME} = "desktop" ]]; then

if [[ ${HOSTNAME} = "laptop" ]]; then

	brightnessctl -d intel_backlight set 70000 
    #swaymsg input 'type:keyboard' xkb_layout us

fi
