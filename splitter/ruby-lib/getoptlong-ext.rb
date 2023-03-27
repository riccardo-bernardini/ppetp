#
# This module extends the standard Ruby module getoptlong by adding
# a new way to specify the option syntax.  The idea is to specify
# the syntax by writing the "help message" printed when a wrong
# command line is detected.
#
# I am going to write a better help in the future, now I simply 
# add an example usage
#
#    #
#    # Create the help message (here a "here doc" is used)
#    #
#
#    syntax = <<EOF
#          Usage: #{File.basename($0)}  [options]
#    
#    s | server-type=text     // type of http server 
#    p | server-path=text     // full path to the server exe
#    P | server-port=int      // port used by the server 
#    C | server-config=path   // path to the generated config file
#    l | log-file=path        // path to the server log file      
#    i | interface=text       // interface server <-> script 
#    w | www-dir=path         // path of the document root        
#    v | verbose              // be verbose
#    EOF
#
#    #
#    # Create a GetoptLong object by using the "syntax string"
#    # above as a parameter.
#    #
#
#    opts = GetoptLong.new(syntax) 
#
#    #
#    # Use the GetoptLong object as usual
#    #
#
#    opts.each do |opt, arg|
#      ...
#    end
#
require 'getoptlong'
 
class GetoptLong 
  alias :vecchia_inizializzazione :initialize
  def initialize(*args)
    if args[0].is_a?(String)
      args, @syntax_message = parse_syntax(args[0])
    else
      @syntax_message = ''
    end

    vecchia_inizializzazione(*args)
  end

  def print_help(msg=nil)
    puts("#{msg}\n") if msg
    puts @syntax_message
  end

  private

  def parse_syntax(syntax)
    lines = syntax.split($/)
    # p lines
    empty_line = /^\s*$/
    separation = lines.find {|x| x=~ empty_line}
    if separation
      idx = lines.index(separation)
      head  = lines[0..idx-1]
      lines = lines[idx+1..-1]
    else
      head = ''
    end

    getopt_array = Array.new
    option_regexp = 
      %r{^\s*((.)\s*\|\s*)?([-_a-zA-Z0-9]+)\s*(=(\S+)\s*)?(//(.*))?$}
    lines.each do |line|
      raise "Wrong '#{line}'" if not line =~ option_regexp
      short_name = $2
      long_name  = $3
      parameter  = $5
      comment    = $7

      entry = ["--" + long_name]
      entry << "-" + short_name if short_name

      entry << if parameter.nil?
		 GetoptLong::NO_ARGUMENT
	       elsif parameter =~ /^\[(.*)\]$/
		 GetoptLong::OPTIONAL_ARGUMENT
	       else
		 GetoptLong::REQUIRED_ARGUMENT
	       end

      getopt_array << entry
    end

    return [getopt_array, syntax]
  end
end
