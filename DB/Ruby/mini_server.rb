#!/usr/bin/env ruby

#
# Nano-wrapper (nano-stuff is so fashionable...) for the internal 
# state DB server.  Useful for testing purposes.  It requires on
# the command line the number of the port
#

if (ARGV.size != 1 || ! (ARGV[0] =~ /^[0-9]+/))
  puts "Usage: #{$0} port"
  exit(1)
end

port = ARGV[0].to_i

#
# Find your own dir and add to LOAD_PATH the directory where the
# require'd files are to be found.
#
my_dir = File.dirname(File.expand_path(__FILE__))
relative_paths = %w(. ../../lib/Ruby)

relative_paths.each do |path| 
  $LOAD_PATH << File.expand_path("#{my_dir}/#{path}") 
end


#
# Load the DB library
#
require 'internal_state'


puts "Starting server..."
server = Internal_State::Server.new
server.listen_to_port(port)

puts "listening to port #{port}"

trap ("QUIT") { exit }
loop do
  sleep 60
end

