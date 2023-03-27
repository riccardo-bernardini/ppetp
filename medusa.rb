#!/usr/bin/env ruby

#
# This Ruby script is the "main" program of MEDUSA. Its duty is 
# (1) to prepare the environment by adding to the RUBYLIB environment 
# variable the paths to the necessary libraries, then (2) load
# the "true" main program implemented in "medusa_root.rb". 
#

require 'rbconfig'
include Config


#
# Where I live
#
my_dir = File.dirname(File.expand_path(__FILE__))

#
# Lib path relative to the directory where this script lives
#
relative_libdirs = %w(lib/Ruby DB/Ruby DSP/Trivial Main)

#
# Absolute paths to the library directories
#
absolute_libdirs = relative_libdirs.map do |path| 
  File.expand_path("#{my_dir}/#{path}")
end

#
# Append the library dirs to the ruby search path 
#
absolute_libdirs.each {|path| $LOAD_PATH << path}

#
# Append the library dirs also to the environment 
# variable RUBYLIB (so it is inherited by our children)
#
new_dirs = absolute_libdirs.join(CONFIG['PATH_SEPARATOR']);

ENV['RUBYLIB'] ||= ""
ENV['RUBYLIB'] += CONFIG['PATH_SEPARATOR'] + new_dirs;

#
# Set also the environment var MEDUSA_HOME to the dir of 
# this script
#
ENV['MEDUSA_HOME'] = my_dir


#
# Load the configuration file
#
require 'read_config'
$config = Config_Data.read(:types => { 
			     'my-port'    => Integer,
			     'db-port'    => Integer,
			     'P2P-bin'    => String,
			     'MMEDIA-bin' => String,
			     'PLAYER-bin' => String },
			   :on_undefined => :die,
			   :files    => my_dir+ '/etc/medusarc')

#
# Wonderful! Now load the main module
#
load 'medusa_root.rb'
