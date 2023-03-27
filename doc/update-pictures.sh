#!/bin/sh
echo $0

formato=eps
recursive=0
while true ; do
    case $1 in
	-r)
	    recursive=1
	    shift;;
	-L*)
	    if [ "$1" = "-L" ]; then
		formato=$2
		shift
		shift
	    else
		formato=${1:2}
		shift
	    fi;;
	*) break;;
    esac
done

echo $recursive

if [ $# -eq 0 ] ; then 
    if [ $recursive -eq 0 ] ; then
	exec sh $0 -r "-L$formato" *.fig
    else
	exit 0
    fi
fi


temp_makefile=`mktemp /tmp/update-fig.XXXXXX`

if [ $? -ne 0 ] ; then
    echo "Could not create temporary Makefile"
    exit 1
fi

cat > $temp_makefile <<EOF
%.$formato: %.fig
	fig2dev -L $formato \$< \$@

EOF

for i do
   j=`basename $i .fig`
   make -f $temp_makefile $j.$formato
done

rm $temp_makefile