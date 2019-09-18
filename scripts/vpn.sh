VPN_CMD=nordvpn

vpn_avail=$(which $VPN_CMD)

if [ -z "$vpn_avail" ]; then
	echo "no ---"
else
	status=$($VPN_CMD status | grep Country | cut -f2 -d:"" | cut -c 2-100)

	if [ -z "$status"  ]; then
		echo "---"
	else
		echo "$status"
	fi;
fi;
