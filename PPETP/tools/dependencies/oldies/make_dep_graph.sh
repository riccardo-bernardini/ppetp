#!/bin/sh

#
# This little script is useful to make a dependency graph of 
# the PPETP source code.  Just go in the directory of this script
# and run it with ./make_dep_graph.sh.  The result will be an EPS
# file in the same directory of this script.
#

#
# Some configuration data
#
my_dir=$PWD                 # The dir of this script
obj_dir=$my_dir/../../obj   # The obj dir
first_file=ppetp-api.adb    # The "root" of the sources

#
# Output format and where the result goes
#
graph_format=ps
result_file=$my_dir/graph.$graph_format
srcdir=$my_dir/../.. 

#
# Make a temp directory where intermediate files will go
#
tmp_dir="/tmp/$$-dep_graph"
mkdir $tmp_dir

xref_output=$tmp_dir/xref_out
dot_file=$tmp_dir/graph.dot

###
### MAIN STUFF
###

cd $obj_dir

echo Creating X-ref...
gnat xref -g -nostdinc -nostdlib $first_file > $xref_output || exit 1

echo ...Converting X-ref to dot...
$my_dir/gnatxref_output_to_dot.rb --srcdir=$srcdir < $xref_output > $dot_file || exit 1

echo ...Converting dot to $graph_format...
dot -T$graph_format $dot_file -o $result_file || exit 1

echo ...done.  
echo You can find the result in "$result_file"
