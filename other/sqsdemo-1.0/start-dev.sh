#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname sqsdemo_dev \
    -s sqsdemo \
    -s reloader
