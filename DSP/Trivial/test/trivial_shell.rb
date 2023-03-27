#!/usr/bin/env ruby

#
# Nano-shell for trivial_mmedia.rb.  For testing purposes.
#

#
# Where I live
#
my_dir = File.dirname(File.expand_path(__FILE__))

#
# Lib path relative to the directory where this script lives
#
relative_libdirs = %w(.. ../../../lib/Ruby ../../../DB/Ruby)

#
# Absolute paths to the library directories
#
absolute_libdirs = relative_libdirs.map do |path| 
  File.expand_path("#{my_dir}/#{path}")
end

#
# Append the library dirs to the ruby search path and to the 
# environment variable (so it is inherited by our children)
#
absolute_libdirs.each {|path| $LOAD_PATH << path}

load "trivial_mmedia.rb"




