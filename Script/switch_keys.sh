#!/bin/bash
curr_layout=$(setxkbmap -query | grep layout)
if [ "$curr_layout" = "layout:     us" ] ; then
    setxkbmap br
else
    setxkbmap us
fi
