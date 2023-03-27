#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

puts "digraph pippo{"

while line=gets
  line.chomp!
  line = line.tr('-', '_');

  from, to = line.split(/ *: */)

  if (to.nil?)
    puts "\t#{from};\n"
  else
    to.split(/, */).each do |dst|
      puts "\t#{from} -> #{dst};\n"
    end
  end
end

puts "}"

