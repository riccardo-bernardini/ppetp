#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

require 'tempfile'

def make_tmp(base)
  temp_file = Tempfile.new(base)
  temp_file.close
  return temp_file.path
end

my_dir  = File.expand_path(File.dirname($0))
obj_dir = File.expand_path("#{my_dir}/../../obj")
src_dir = File.expand_path("#{my_dir}/../..")
root    = "ppetp-api.adb"
converter = "#{my_dir}/gnatxref_output_to_dot.rb"

graph_format = "ps"
result_file  = "#{my_dir}/graph.#{graph_format}"

xref_file = make_tmp('xref_file')
dot_file  = make_tmp('dot_file')

Dir.chdir(obj_dir)

$stderr.puts "Creating X-ref..."
ok=system("gnat xref -g -nostdinc -nostdlib #{root} > #{xref_file}")
exit 1 unless ok

$stderr.puts "...Converting X-ref to dot..."
ok=system("#{converter} --srcdir=#{src_dir} < #{xref_file} > #{dot_file}")
exit 1 unless ok

$stderr.puts "...Converting dot to #{graph_format}..."
ok=system("dot -T#{graph_format} #{dot_file} -o #{result_file}")
exit 1 unless ok

$stderr.puts "...done."
$stderr.puts "You can find the result in #{result_file}"

