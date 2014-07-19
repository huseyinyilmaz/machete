#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/apps/web/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s machete
