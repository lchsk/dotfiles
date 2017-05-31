#!/usr/bin/env sh

sudo netctl restart `netctl list | grep \* | sed -e "s/* //"`
