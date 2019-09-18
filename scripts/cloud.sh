CLOUD_CMD=dropbox

cloud_avail=$(which $CLOUD_CMD)

if [ -z "$cloud_avail" ]; then
	echo "no cloud"
else
	status=$($CLOUD_CMD status)

	if [ -z "$status" ]; then
		echo "---"
	else
		echo "$status"
	fi;
fi;
