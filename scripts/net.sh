conn_name () {
	netctl list | grep -E -o "\* ([a-zA-Z0-9]+)" | sed -e "s/* //"
}

conn_name
