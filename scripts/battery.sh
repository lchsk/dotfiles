batt=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | grep -E -o "[0-9]{1,3}%")

if [ -z "$batt" ]; then
	echo "--"
else
	echo "$batt"
fi;
