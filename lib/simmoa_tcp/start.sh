#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/../simmoa-0.1.0/ebin -boot start_sasl -s simmoa_tcp
