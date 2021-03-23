#!/bin/bash
#   simple ffe start script

stty --f /dev/tty icanon raw -echo
erl -noshell -s ffe
stty echo echok icanon -raw

