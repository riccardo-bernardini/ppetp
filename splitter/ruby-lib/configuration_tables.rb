=begin rdoc

  = What is this?
  This file provides class Configuration_Table that can be useful to
  handle the "configuration" of a program.  An object of type
  Configuration_Table can get configuration data from config files, 
  enviroment variables and command line options.

  = How do I use it?
  === Object creation
  First you need to create a Configuration_Table object.  You can
  give to the constructor default configuration values, name of files
  to be read, the name of a environment variable to be read and so
  on...   For example,

       config = Configuration_Table.new('defaults' => {
                                          'input-ports'  => [5004],
                                          'timeout'      => 5.0,
                                          'server-port'  => 42430,
                                        },
                                        'env-var'  => 'PARSE_ME',
                                        'parsers'  => {
                                          'input-ports' => '#,int',
                                          'timeout'     => 'float',
                                          'server-port' => 'int'
                                        },
                                        'option-settable' => true)

  === Using the object

  Now you can use +config+ as an +Hash+ table. For example, you can 
  write <tt>config['input-ports']</tt> to get the value of parameter
  <tt>input-ports</tt>.  

  === Command line options parsing

  A +Configuration_Table+ object can read parameter also from command
  line options.  To do this you need only to call method
  +parse_options+ passing to it the same parameter that you would pass
  to <tt>GetoptLong.new</tt>.  +parse_options+ will return a vector 
  of [option, argument] pairs containing the option that were not
  recognized. 

  = Configuration file syntax

  A configuration file is expected to have the following structure
  
  * Any line whose first non-blank character is '#' is considered a
    comment line
  * Any non-comment (and non-empty) line must have the following
    structure 

           option-name '=' option-value

    where option-name is any string without '=' inside and
    option-value includes all the char up to the end of the line

=end

require 'getoptlong'

#
#  This class is useful to handle the configuration of a program.
#  An object of this class can be accessed as an Hash that maps
#  configuration parameter names into values.  A Configuration_Table
#  can be initialized by several means
#
#  * Default values can be given at creation time
#  * Parameter can be read from 
#     - configuration files
#     - environment variables
#     - command-line options
#
class Configuration_Table
=begin rdoc
  
    The constructor accepts several types of arguments.  In order to
    gain flexibility, the arguments are given in a "nominative" way
    (similar to what is done in Ada); for example,
  
        Configuration_Table.new('defaults' => {
                                    'input'  => '/dev/null',
                                    'port'   => 5555,
                                    'output' => '/dev/null'
                                },
                                'parsers' => {
                                    'port'   => 'int'
                                })
  
    The accepted parameters are
  
    [defaults]  The value is an Hash table that it is used to
                initialize the Configuration_Table
                object. Acceptable synonimes: +default+
    [config-file] The name of a configuration file to be read. This
                  parameter can also be an array of names; in this
                  case the files are read in the order given in the
                  array (so that the last files have larger
                  priority).  See in the following for the syntax of
                  the configuration file.
    [environment-variable] The name of an environment variable to be
                           read.  See in the following for the syntax of
                           the string in the environment
                           variable. Alternative names: "environment"
                           can be replaced by "external"; moreover, 
                           "environment" can be shortened to "env",
                           "external" can be shortened to "ext",
                           and "variable" can be shortened to "var"
                           (e.g., env-var, ext-var, ext-variable are
                           all possible synonimes).
    [parsers] Parameter values can have types different from plain
              strings (e.g., a port number is represented by an
              integer). If desired, the mapping from string to the
              final type can be done by the Configuration_Table
              itself.  The value of this parameter is an Hash table
              that maps parameter names to "parsers"; a parser can be
              * A "callable" object, that is, an object that respond
                to method +call+.  The string is given as parameter to
                the callable object and the result is stored in the 
                table.  For example,
                       'port' => proc {|x| x.to_i} 
                can be used to convert the parameter to an integer
                value
              * For very common types (e.g., integers, floats, ...)
                some builtin parsers are provided.  In this case one
                specifies the parser by giving its name.  Currently
                defined builtin parsers are
                   - 'int'
                   - 'float'

                                      
=end
  def initialize(argument_list)
    @config_data = Hash.new
    @accepted_options = nil
    @parsers = nil
    @option_settable = []
    
    config_file = nil
    environment_var = nil

    argument_list.each do |key, value|
      case key.downcase
      when /defaults?/
	@config_data = value
      when /accepted([- ]options)?/
	@accepted_options = value
      when /config[- ]file/
	config_file = value
      when /(ext(ernal)?|env(ironment)?)[- ]var(iable)?/
	environment_var = value
      when 'parsers'
	@parsers = value
      when /^option[- _]settable$/
	@option_settable = value
      else
	raise "Unrecognized option #{key}"
      end
    end

    if not config_file.nil?
      config_file = [config_file] unless config_file.is_a? Array

      config_file.each { |filename| load(filename) }
    end

    use_environment_var(environment_var) unless environment_var.nil?
  end

  #
  # Load a configuration file.  Parameter +input+ can be both a string
  # (interpreted as filename) or any object that responds to +each+
  # and such that at every iteration a new line is returned.  This
  # allows reading configuration data from any +IO+ object (e.g., a
  # pipe) and even arrays of strings.
  #
  def load(input)
    stream = if input.is_a?(String) 
	       File.open(input)
	     else 
	       input 
	     end

    stream.each do |line|
      line.chomp!
      next if line =~ /^\s*(#.*)?$/

      option, value = line.split('=', 2)
      if value.nil?
	raise "Invalid line '#{line}' at line #{input.lineno} in #{path}"
      end

      key = unescape(option).downcase
      raise "Unrecognized option #{option}" unless valid?(key)
      
      self.parse_and_set(key, value)
    end

    stream.close unless input.is_a? IO
  end

  def parse_config_line(config_line)
    config_line.split('&').each do |entry|
      option, value = entry.split('=', 2)

      if value.nil?
	case option
	when /^@(.*)$/
	  load($1)
	else
	  raise "Invalid entry '#{entry}' in config line"
	end
      else
	key = unescape(option)
	raise "Unrecognized option #{option}" unless valid?(key)
	value = unescape(value)
	self.parse_and_set(key, value)
      end
    end
  end

  def use_environment_var(var_name)
    if ENV.has_key?(var_name)
      self.parse_config_line(ENV[var_name])
    end
  end

  def use_option(opt, arg)
    if (@option_settable==true || @option_settable.include?(opt))
      parse_and_set(opt, arg)
      return true
    else
      return false
    end
  end

  def parse_options(opt_object)
    if not opt_object.is_a?(GetoptLong)
      opt_object = GetoptLong.new(opt_object)
    end

    unparsed_options = []
    opt_object.each do |opt, arg|
      opt =~ /^-+(.*)$/
      if not self.use_option($1, arg)
	unparsed_options << [opt, arg]
      end
    end

    return unparsed_options
  end

  # Return an hash table with the configuration data
  def to_hash
    return @config_data.dup
  end

  # Return the value associated with the configuration parameter +key+
  def [](key)
    @config_data[key]
  end

  # Set the value associated with the configuration parameter
  # +key+. If +value+ is a string, it is parsed using the parsers
  # specified at construction time, otherwise +value+ is stored as it
  # is. 
  def []=(key, value)
    if value.is_a? String
      value = apply_parser(key, value)
    end

    @config_data[key]=value
  end

  # Parse +value+ with the parsers specified at construction time and
  # assign the value to parameter +key+
  def parse_and_set(key, value)
    @config_data[key]=apply_parser(key, value)
  end

  # Assign +value+ to parameter +key+ *without* any parsing.
  def set_raw(key, value)
    @config_data[key]=value
  end

  private

  def valid?(key)
    return (@accepted_options.nil? || @accepted_options.include?(key))
  end

  def unescape(string)
    string.tr('+', ' ').gsub(/((?:%[0-9a-fA-F]{2})+)/n) do
      [$1.delete('%')].pack('H*')
    end
  end

  @@basic_parsers = {
    'int'   => Proc.new {|x| x.to_i},
    'float' => Proc.new {|x| x.to_f},
	 'text'  => Proc.new {|x| x.to_s}
  }

  def apply_parser(key, value)
    #
    # If no parser is defined for key, keep value as it is
    #
    p key + " " +value
    return value unless (@parsers && @parsers.has_key?(key))
    parser = @parsers[key]

    #
    # A parser can be:
    #
    #   * Something "callable" (i.e., it responds to 'call')
    #
    #   * A string describing the format.  If the string begins
    #     with '#', it means a list of value.  For example,
    #     '#,int' means a comma-separated list of integers while
    #     '#&float' represents a '&'-separated list of floats
    #
    if (parser.respond_to?('call'))
      return parser.call(value)
    elsif (parser.is_a?(String))
      if parser[0..0] == '#'
	separator = parser[1..1]
	parser = @@basic_parsers[parser[2..-1]]
	raise "Invalid parser" if parser.nil?

	return value.split(separator).map {|x| parser.call(x)}
      else
	parser = @@basic_parsers[parser]
	raise "Invalid parser" if parser.nil?

	return parser.call(value)
      end
    else
      raise "Invalid parser"
    end
  end

end
