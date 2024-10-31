#!/bin/bash

# Get the name of the workspace currently on DP-5
workspace_on_dp5=$(swaymsg -t get_workspaces | jq -r '.[] | select(.output == "DP-5") | .name')

# Check if the workspace_on_dp5 variable is not empty
if [ -n "$workspace_on_dp5" ]; then
    # Move the currently focused window to the workspace on DP-5
    swaymsg move container to workspace "$workspace_on_dp5"
else
    echo "DP-5 not found or no workspace assigned to DP-5."
fi
