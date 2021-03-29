#!/bin/bash
#   simple ffe start script

if [ -t 0 ]; then
    exec erl -s ffe -noinput -- $0 $*
else
    exec erl -s ffe -noshell -- $0 $*
fi
