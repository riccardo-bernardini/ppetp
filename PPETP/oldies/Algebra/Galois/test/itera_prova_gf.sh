#!/bin/sh

good=""
bad=""
all_good=1
good_cases=/tmp/good.txt
bad_cases=/tmp/bad.txt
date > $good_cases
date > $bad_cases
#  
# if which tempfile 2>/dev/null ; then
#     tempfile=`tempfile`
# else
#     while true ; do
# 	tempfile="/tmp/gftest-$$"
# 	if [ ! -f $tempfile ] ; then
# 	    touch $tempfile
# 	    break
# 	fi
#     done
# fi

from=""
if [ $# -gt 0 ]; then
    if echo "$1" | grep [0-9] >/dev/null ; then
	from="$1"
    fi
    shift
fi

to=""
if [ $# -gt 0 ]; then
    if echo "$1" | grep [0-9] >/dev/null ; then
	to="$1"
    fi
    shift
fi

if [ -z "$from" ]; then
    from=1;
fi

if [ -z "$to" ]; then
    to=32;
fi


tempfile=/tmp/lavori-in-corso
i=$from
while [ $i -le $to ]; do
    echo $i > $tempfile
    sed -r -e "s/ [0-9]+;--###/ $i;/" < prova_gf_2p.adb > vero_prova_gf.adb
    if gnatmake -I.. -gnata vero_prova_gf ; then
	:
    else
	exit;
    fi

    if ./vero_prova_gf ; then
	good="$good $i"
	echo $i >> $good_cases
    else
	bad="$bad $i"
	all_good=0
	echo $i >> $bad_cases
    fi

    rm ./vero_prova_gf
    rm ./vero_prova_gf.o

    i=`expr $i + 1`
done

echo "Buoni: $good"
echo "Cattivi: $bad"
echo "tutti buoni: $all_good"