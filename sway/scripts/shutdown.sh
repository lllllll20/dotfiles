#! /usr/bin/bash

if [[ ${HOSTNAME} = "desktop" ]]; then
    systemctl suspend
elif [[ ${HOSTNAME} = "laptop" ]]; then
    poweroff
fi
