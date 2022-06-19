#!/usr/bin/env python3

# This example shows how to run a command when i3 exits
#
# https://faq.i3wm.org/question/3468/run-a-command-when-i3-exits/

# This is the command to run

import subprocess
import i3ipc
import os

HOME = os.getenv("HOME")
scriptpath = HOME + '/.config/sway/scripts/viewlast.py'

COMMAND = ['python', scriptpath, '--switch']

def on_winclose(i3, e):
    tree = i3.get_tree()
    nws = set(w.id for w in tree.find_focused().workspace().leaves())
    if len(nws) < 1:
        subprocess.Popen(COMMAND, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


i3 = i3ipc.Connection()

i3.on('window::close', on_winclose)

i3.main()
