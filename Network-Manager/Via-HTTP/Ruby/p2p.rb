#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

#
# ===================
# == What is this? ==
# ===================
#
# This is a small Ruby script (callable via CGI) that implements a 
# PPETP network manager to be used for experimental purposes.  This
# CGI script expect to be called via a GET or a POST method with query 
# part with the following structure:
#
#  * fields "command" (what to do), "stream" (the multimedia stream) 
#    and "server" are always present
#
#  * Field "command" can assume the following values
#
#        - "create_stream"
#
#        - "destroy_stream"
#
#        - "join_user"
#
#        - "remove_user"
#
# See file protocol_notes.txt for more information about the
# other parameters.
#
#
# ===================
# == Configuration ==
# ===================
#
#
# -----------------
# -- Environment --
# -----------------
#
# This script expects to find in its own directory
#
#   * A directory cgi-manager with its library of Ruby sources
#
#   * An optional configuration file 'p2p.config'
#
# -----------------
# -- Config file --
# -----------------
# 
# At startup this script reads the configuration file 'p2p.config'
# that is searched in the same directory of this script. The 
# configuration file is parsed  one line at time.  Empty lines 
# or lines that have '#' as  first non-space char are ignored; 
# other lines should have the form 
# 
#      name=value
#
# or 
#
#      name
#
# where 'name' is any string with no '=' and 'value' is any string
# (but spaces at the beginning and the end of name and value are 
# ignored).  See inside the function parse_config_file in the following
# for more information about the accepted configuration options.
#

my_dir = File.dirname(File.expand_path($0));
$: << my_dir

require 'cgi-manager/utilities'
require 'cgi-manager/config'
require 'cgi-manager/cgi_utilities'
require 'cgi-manager/query_processor'
require 'cgi-manager/net_manager_db'
require 'cgi-manager/allocators'
require 'cgi-manager/authenticators'
require 'cgi-manager/formatters'
require 'cgi-manager/entities'

Config::set('my-dir', my_dir)

def set_config_default
  Config::set('db-type',        'file:filename=/tmp/p2p-net-db.txt')
  Config::set('auth-type',      'null')
  Config::set('allocator-type', 'simple')
  Config::set('interface-mode', 'cgi')
  Config::set('output-format',  'xml')
  Config::set('log-file',       $stderr)
end

def parse_config_file
  #
  # Read the configuration file 'p2p.config' that is in the same 
  # directory of this script.  The configuration file is parsed
  # one line at time.  Empty lines or lines that have '#' as
  # first non-space char are ignored; other lines should have the
  # form 
  # 
  #      name=value
  #
  # or 
  #
  #      name
  #
  # where 'name' is any string with no '=' and 'value' is any string
  # (but spaces at the beginning and the end of name and value are 
  # ignored)
  #
  #
  # The following options are currently implemented
  #
  #   * Option 'db-type'
  #       Its value has the form <db_type>:<db_parameters>
  #       Implemented db_type values
  #         - 'file'  
  #              The DB is stored in a file somewhere. db_parameters
  #              has the form of a query, that is, string of form
  #              'param=value' (possibly '%'-quoted) separated by '&'.
  #              Acceptable values for 'param' are
  #
  #                + 'filename'  This parameter is mandatory and its
  #                     value is the path of the file holding the DB
  #
  #                + 'mode'  This parameter is optional. It specifies
  #                     the format used to store the DB.  Currently
  #                     implemented modes are 'yaml' (text based),
  #                     'marshal' (binary), 'text' (equivalent to 'yaml')
  #                     and 'binary' (equivalent to 'marshal').  Binary
  #                     mode is faster, but text mode can be read by
  #                     humans too.  Default is text mode.
  #
  #      Example:
  #
  #           db-type=file:filename=/tmp/a.txt&mode=binary              
  #
  config_entries = { 
    'db-type'         => nil,    
    'auth-type'       => nil,
    'allocator-type'  => nil,
    'output-format'   => nil,
    'interface-mode'  => nil,
    'log-file'        => Proc.new {|x| File.open(x, 'w')}
  }

  filename = Config::get('my-dir') + '/p2p.config'

  return unless FileTest.readable?(filename)

  File.open(filename) do |stream|
    stream.each do |line|
      line.chomp!
      next if line =~ /^\s*(#.*)?$/
      line =~ /^\s*([^=]*\S)\s*(=\s*(.*\S))?\s*$/
      name  = $1
      value = ($3 || true)
      die("Unknown config option '#{name}' at line #{stream.lineno}") unless 
	config_entries.has_key?(name)

      action = config_entries[name]
      case action
      when nil
	Config::set(name, value)
      when Proc
	Config::set(name, action.call(value))
      else
	die("Internal: unknown action #{action}")
      end
    end
  end
end

def do_logging(msg, level=1)
  Config::get('log-file').puts(msg)
end

def die(msg)
  do_logging("Error: #{msg}")
  exit 1
end

def get_db(type)
  result = NetManager_DB::get_db_handler(type)
  die("Unimplemented DB mode #{type}") if result.nil?
  return result
end

def get_allocator(type)
  result = Allocators::get_allocator(type)
  die("Unimplemented allocator '#{type}'") if result.nil?
  return result
end

def get_formatter(type)
  result = Formatters::get_formatter(type)
  die("Unimplemented formatter '#{type}'") if result.nil?
  return result
end

def get_authenticator(type)
  result = Authenticators::get_authenticator(type)
  die("Unimplemented authenticator '#{type}'") if result.nil?
  return result
end

module Query_Reader
  @@n_calls = 0

  #
  # Return a string relative to the next query or nil if no other query
  # is available.  Parameter mode selects the mode this is script 
  # interfaces with the web server.
  # Currently only CGI mode is implemented. In CGI mode the second
  # call returns nil
  #
  def Query_Reader::next_query(mode='cgi')
    case mode
    when 'cgi'
      return Query_Reader::next_cgi_query
    else
      raise "Boh"
    end
  end

  private
  def Query_Reader::next_cgi_query
    return nil if @@n_calls > 0

    @@n_calls += 1

    die("REQUEST_METHOD unset?!?" ) if ENV['REQUEST_METHOD'].nil?

    result = nil
    case ENV['REQUEST_METHOD'].upcase
    when 'POST'
      result = $stdin.read.chomp
    when 'GET'
      if ENV['QUERY_STRING'].nil?
	die("GET with QUERY_STRING unset") 
      else
	result = ENV['QUERY_STRING']
      end
    else
      die("Invalid value for REQUEST_METHOD")
    end

    return result
  end
end

####################
### MAIN PROGRAM ###
####################

set_config_default

parse_config_file



authenticator = get_authenticator(Config::get('auth-type'))
stream_db     = get_db(Config::get('db-type'))
allocator     = get_allocator(Config::get('allocator-type'))
formatter     = get_formatter(Config::get('output-format'))

processor = Processor.new(stream_db, allocator)

while query = Query_Reader::next_query(Config::get('interface-mode'))
  param = CGI::parse(query)

  reply = if (not authenticator.check_query(query, param))
	    CGI::Reply.new(CGI::Forbidden)
	  else	  
	    result = processor.handle_request(param)
	    case result
	    when nil
	      CGI::Reply.new(CGI::OK)
	    when Peer_List
	      data, data_type = formatter.make_config_data(result)
	      CGI::Reply.new(CGI::OK, data, data_type)
	    when NetManager_DB::DB_Error
	      CGI::Reply.new([CGI::Bad_Request, "DB error: #{result.to_s}"])
	    when Processor::Bad_Command
	      CGI::Reply.new([CGI::Bad_Request, "syntax"])
	    when Allocators::No_Peers
	      CGI::Reply.new([CGI::Service_Unavailable, "Not enough peers"])
	    else
	      raise result
	      CGI::Reply.new([CGI::Internal_Error, "A456-x2"])
	    end
	  end

  CGI::write(reply)
end

