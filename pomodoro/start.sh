#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname pomodoro_dev \
    -s pomodoro \
    -s reloader
