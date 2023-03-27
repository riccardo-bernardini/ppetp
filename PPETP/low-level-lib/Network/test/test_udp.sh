#!/bin/sh

gnatmake -q -Ptest test_udp

(sleep 2; obj/test_udp) &
a=`binaux/test_udp.rb`

if [ "$a" == 1:2:3:5:8 ] ; then
    echo SUCCESS
    exit 0
else
    echo FAILURE
    exit 1
fi


