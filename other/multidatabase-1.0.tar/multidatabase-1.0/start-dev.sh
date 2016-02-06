#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname multidatabase_dev \
    -s multidatabase \
    -s reloader
