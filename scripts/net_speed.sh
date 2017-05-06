netspeed () {
    CONN=$(~/dotfiles/scripts/net.sh)
    awk '/'"$CONN"'/ {i++; rx[i]=$2; tx[i]=$10}; END{down=sprintf("%.0f kB/s", (rx[2]-rx[1])/1000); printf down; printf " "; up=sprintf("%.0f kB/s", (tx[2]-tx[1])/1000); print up;}' <(cat /proc/net/dev; sleep 1; cat /proc/net/dev)
}

netspeed
