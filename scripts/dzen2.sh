dzen_avail=$(which dzen2)

if [ -z "$dzen_avail" ]; then
	echo "~/bin/dzen2"
else
	echo "$dzen_avail"
fi;
