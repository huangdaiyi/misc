#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname chat_room_dev \
    -s chat_room \
    -s reloader
