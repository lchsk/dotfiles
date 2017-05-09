amixer -D pulse get Master | grep -E -o "([0-9]{1,3}%).+?(on|off)" | sed -r -e "s/].*?\[/ /" | sed -e "s/off/mute/" | sed -e "s/ on//" | uniq
