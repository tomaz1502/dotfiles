#!/bin/sh

if [ $1 = "UP" ]; then
    amixer -D pulse sset Master 5%+
else
    amixer -D pulse sset Master 5%-
fi
