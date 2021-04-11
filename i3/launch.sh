#!/usr/bin/env bash

dir="$HOME/.config/polybar"
themes=(`ls --hide="launch.sh" $dir`)

launch_bar() {
	# Terminate already running bar instances
	killall -q polybar

	# Wait until the processes have been shut down
	while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

	# Launch the bar
	if [[ "$style" == "hack" || "$style" == "cuts" || "$style" == "forest" ]]; then
		# polybar -q top -c "$dir/$style/config.ini" &
		# polybar -q bottom -c "$dir/$style/config.ini" &
        if type "xrandr"; then
          for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
            MONITOR=$m polybar --reload -c "$dir/$style/config.ini" -q bottom &
            MONITOR=$m polybar --reload -c "$dir/$style/config.ini" -q top &
          done
        else
          polybar --reload -c "$dir/$style/config.ini" -q bottom &
          polybar --reload -c "$dir/$style/config.ini" -q top &
        fi
	elif [[ "$style" == "pwidgets" ]]; then
		bash "$dir"/pwidgets/launch.sh --main
	else
		polybar -q main -c "$dir/$style/config.ini" &	
	fi
}

style="forest"
launch_bar
