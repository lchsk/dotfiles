conn_name () {
	# netctl list | grep -E -o "\* ([a-zA-Z0-9]+)" | sed -e "s/* //"
	ip link | grep "state UP" | cut -f2 -d: | awk '{print $1}'
}

conn_name
