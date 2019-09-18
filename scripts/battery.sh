batt=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep percentage | grep -E -o "[0-9]{1,3}")

if [ -z "$batt" ]; then
	echo "--"
else
	if [ "$batt" -gt 50 ]; then
		echo "$batt%"
	elif [ "$batt" -gt 25 ]; then
		echo "--------- BATTERY LOW --------- $batt%"
	else
		echo "-------------------- BATTERY REALLY LOW -------------------- $batt%"
	fi;
fi;
