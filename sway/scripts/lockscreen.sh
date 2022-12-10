#! /usr/bin/bash

WAYLAND_DISPLAY=wayland-1
XDG_RUNTIME_DIR=/run/user/$(id -u)
export XDG_RUNTIME_DIR
export WAYLAND_DISPLAY

playerctl pause
swaylock -u -i ~/computing/wallpapers/"245.jpg"
