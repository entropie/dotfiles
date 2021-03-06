#!/bin/sh
#
# (c) 2007 by Robert Manea
#
# display download progress and cancel download 
# on button3 click
#

(wget --progress=dot "$@" 2>&1; echo "Download(s) finished.."; sleep 5) | \
    dzen2 -ta l -w 600 -fn '-*-fixed-*-*-*-*-16-*-*-*-*-*-*-*' \
    -e 'button3=exec:pkill -15 wget,exit'
