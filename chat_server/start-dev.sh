#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname chat_server_dev \
    -s chat_server \
    -s reloader
