#! /usr/bin/bash

sleep 1

swaymsg [app_id="files"] resize set width 1300 height 700, move position center
swaymsg [app_id="floatterm"] resize set width 1300 height 700, move position center
swaymsg [app_id="music"] resize set width 1300 height 700, move position center
