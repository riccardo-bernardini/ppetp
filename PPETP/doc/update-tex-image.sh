#!/bin/bash

function resolve_link {
    name=$1
    max_links=10
    while [ $max_links -gt 0 -a -L "$name" ] ; do
	name="`readlink $name`"
	let max_links--
    done

    if [ -L "$name" ] ; then
	echo Too many symbolic links on "$1"  >& 2 
	exit 1
    fi

    echo $name
}

function make-verbatim {
echo '\begin{verbatim}'
cat
echo '\end{verbatim}'
}

src=`resolve_link "$1"`
dst=`resolve_link "$2"`

if [ ! -e "$src" ] ; then
    echo "File '$src' not found"
    exit 1
fi

if [ ! -e "$dst" -o "$src" -nt "$dst" ] ; then
    make-verbatim < $src > $dst
fi




