#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

require 'getoptlong'
require 'rbconfig'

$my_dir = File.dirname(File.expand_path($0));
$: << $my_dir


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

class Embedded_File
  #
  # This is a nice class that allows to use the DATA constant as a
  # "virtual filesystem".  You just open a "virtual file" with
  #
  #                   f = Embedded_File.new(path)
  #
  # then you can use f with most of the IO based reading functions 
  # (i.e., getc, gets, each, ....)
  #

  #
  # @@files will hold an Hash table mapping filenames to content
  #
  @@files = nil

  def Embedded_File.use(source)
    @@files = Hash.new
    data = source.read
    return if data.empty?
    while not data.empty?
      #
      # Separate the header line from the rest of data
      #
      header, data = data.split($/, 2)
      raise IOError if data.nil?

      #
      # Get the "EOF marker" and the filename
      #
      marker, name = header.split(/ +/, 2)

      #
      # Separate content and rest of data
      #
      content, data = data.split(marker + $/, 2)
      raise IOError if data.nil?

      @@files[name]=content
    end
  end


  def Embedded_File.init
    Embedded_File.use(DATA) if @@files.nil?
  end

  def Embedded_File.dir
    Embedded_File.init
    return @@files.keys
  end

  def initialize(path)
    Embedded_File.init

    raise Errno::ENOENT.new(path) unless @@files.has_key?(path)

    @data = @@files[path]
    @path = path
    @next_char = 0
    @open = true
  end

  def getc
    raise IOError unless @open
    return nil if eof?

    result = @data[@next_char]
    @next_char += 1 unless result.nil?
    return result
  end

  def gets(separator=$/)
    raise IOError unless @open
    return nil if eof?

    @data[@next_char..-1].index($/)
    line_length = @data[@next_char..-1].index($/)

    eol = line_length.nil? ? (@data.size-1) :  (@next_char + line_length)

    result = @data[@next_char..eol]

    @next_char = eol+1

    return result
  end

  def closed?
    return !@open
  end

  attr_reader :path

  def eof?
    raise IOError unless @open
    return @next_char >= @data.size
  end

  alias :eof :eof?

  def read(length=nil)
    raise IOError unless @open
    length = @data.size - @next_char if length.nil?
    last_char = @next_char + length-1
    result = @data[@next_char..last_char]
    @next_char = last_char+1

    return result
  end

  def seek(where, from = IO::SEEK_SET)
    raise IOError unless @open
    where = case from
	    when IO::SEEK_CUR
	      where + @next_char
	    when IO::SEEK_END
	      where + @data.size - 1
	    when IO::SEEK_SET
	      where
	    else
	      raise Errno::EINVAL
	    end

    @next_char = [[where, 0].max, @data.size].min
  end

  def pos
    raise IOError unless @open
    @next_char
  end

  alias :tell  :pos 

  def close
    raise IOError unless @open
    @open = false
  end

  def each_line(sep=$/)
    while line=gets(sep)
      yield(line)
    end
  end

  alias :foreach :each_line 
  alias :each    :each_line 

  def each_byte
    while c=getc
      yield(c)
    end

    return nil
  end

  def readline(sep=$/)
    raise EOFError if eof?
    gets(sep)
  end

  def readchar
    raise EOFError if eof?
    getc
  end

  def readlines(sep=$/)
    result = []
    each_line(sep) {|x| result << x}
    return result
  end

  def rewind
    seek(0)
  end

  def pos=(where)
    seek(where)
  end

  def Embedded_File.open(path)
    file = Embedded_File.new(path)
    return file if not block_given? 

    result = yield(file)
    file.close
    return result
  end
end


def apply_template(string, var={}, delim=['<?', '?>'])
  open, close = delim
  accum = ''
  while not string.empty?
    head, string = string.split(open, 2)
    return accum + head if string.nil?

    body, string = string.split(close, 2)
    raise "Invalid template" if string.nil?

    accum << head

    if (body =~ /^:([-a-zA-Z_0-9]+):$/)
      accum << var[$1].to_s
    else
      accum << eval(body)
    end
  end

  return accum
end

def die(msg)
  $stderr.puts("Error: #{msg}")
  exit
end

def unescape(string)
  string.tr('+', ' ').gsub(/((?:%[0-9a-fA-F]{2})+)/n) do
    [$1.delete('%')].pack('H*')
  end
end

def parse(query)
  params = Hash.new

  query.split(/[&;]/n).each do |pairs|
    key, value = pairs.split('=',2).collect{|v| unescape(v) }
    key=key.downcase
    params[key] = value
  end
  
  return params
end

def load_config_file(path)
  File.open(path) do |input|
    input.each do |line|
      line.chomp!
      next if line =~ /^\s*(#.*)?$/
      option, value = line.split('=', 2)
      if value.nil?
	die("Invalid line '#{line}' at line #{input.lineno} in #{path}") 
      end

      key = option_to_key(unescape(option).downcase)
      die("Unrecognized option #{option}") if key.nil?

      $config[key]=value
    end
  end
end

def process_config_line(config_data)
  config_data.split('&').each do |entry|
    option, value = entry.split('=', 2)
    if value.nil?
      case option
	when /^@(.*)$/
	load_config_file($1)
      else
	die("Invalid entry '#{entry}' in config line")
      end
    else
      key = option_to_key(unescape(option))
      die("Unrecognized option #{option}") if key.nil?
      value = unescape(value)
      $config[key]=value
    end
  end
end

Server_Info = Struct.new("Server_Info", "exe_name", "command_template")

Server_Type_To_Info = {
  'lighttpd' => Server_Info.new('lighttpd', '<?:exe:?> -f <?:config:?>'),
  'apache'   => Server_Info.new('httpd',    '<?:exe:?> -f <?:config:?>'),
}

def server_type_to_path(exe_name)

  candidate_dirs = ENV['PATH'].split(Config::CONFIG['PATH_SEPARATOR'])

  system_dirs = case Config::CONFIG['host_os'] 
		when /[lL]inux/, /[uU]nix/
		  %w(/usr/sbin /usr/local/sbin)
		else
		  []
		end

  system_dirs.each do |dir|
    unless candidate_dirs.include?(dir)
      candidate_dirs << dir
    end
  end

  candidate_dirs.each do |dir|
    path = File.join(dir, exe_name)
    return path if FileTest.exist?(path)
  end

  die("Could not find exe for #{$config['server_type']}. Use --server-path")
end

def make_command_line(template)
  var = {
    'exe'    =>  $config['server-path'],
    'config' =>  $config['server-config-file']
  }

  return apply_template(template, var)
end

Option_to_Config_Key = {
  'server-type'   => nil,
  'server-path'   => nil,
  'server-port'   => 'port',
  'interface'     => nil,
  'www-dir'       => nil,
  'log-file'      => nil,
  'server-config' => 'server-config-file'
}

def option_to_key(option)
  return nil if not Option_to_Config_Key.has_key?(option)
  return Option_to_Config_Key[option] || option
end

##########
## MAIN ##
##########

$config = {
  'server-type'        => 'lighttpd',
  'interface'          => 'cgi',
  'port'               => '3000',
  'server-config-file' => '/tmp/conf',
  'www-dir'            => File.join($my_dir, 'www'),
  'ruby-path'          => File.join(Config::CONFIG['bindir'], 'ruby'),
  'log-file'           => "/tmp/ht.log"
}

syntax = <<EOF
      Usage: #{File.basename($0)}  [options]

s | server-type=text     // type of http server (apache, lighttpd)
p | server-path=text     // full path to the server exe
P | server-port=int      // port used by the server (default: <?:port:?>)
C | server-config=path   // path to the generated config file (<?:server-config-file:?>)
l | log-file=path        // path to the server log file       (<?:log-file:?>)
i | interface=text       // interface server <-> script (cgi, fcgi, ...)
w | www-dir=path         // path of the document root         (<?:www-dir:?>)
v | verbose              // be verbose
EOF

syntax = apply_template(syntax, $config)

if false
  opts = GetoptLong.new(['--server-type', '-t', GetoptLong::REQUIRED_ARGUMENT],
			['--server-path', '-p', GetoptLong::REQUIRED_ARGUMENT],
			['--server-port', '-P', GetoptLong::REQUIRED_ARGUMENT],
			['--server-config', '-C', GetoptLong::REQUIRED_ARGUMENT],
			['--log-file',    '-l', GetoptLong::REQUIRED_ARGUMENT],
			['--config-file', '-c', GetoptLong::REQUIRED_ARGUMENT],
			['--interface',   '-i', GetoptLong::REQUIRED_ARGUMENT],
			['--www-dir',     '-w', GetoptLong::REQUIRED_ARGUMENT],
			['--verbose',     '-v', GetoptLong::NO_ARGUMENT]
			);
else
  opts = GetoptLong.new(syntax)
  opts.print_help
  exit
end



$verbose = false


Config_Env_Variable = 'PPETP_HTTP_SERVER_CONFIG'
if ENV.has_key?(Config_Env_Variable)
  config_data = ENV[Config_Env_Variable]
  process_config_line(config_data)
end


opts.each do |opt, arg|
  key = option_to_key(opt[2..-1])
  if key
    $config[key]=arg
  else
    case opt
    when '--config-file'
      load_config_file(arg)
    when '--verbose'
      $verbose = true
    else
      raise "bingo!"  # TODO: be more serious
    end
  end
end

required = %w(server-type interface www-dir port)
missing  = []
required.each do |opt|
  missing << opt if not $config.has_key?(opt)
end

die("Missing mandatory parameters: #{missing.join(' ')}") if not missing.empty?

server_info = Server_Type_To_Info[$config['server-type']]
die("Unsupported server type '#{$config['server-type']}'") if server_info.nil?


if not $config.has_key?('server-path')
  $config['server-path'] = server_type_to_path(server_info.exe_name)
end


server = File.basename($config['server-type'])
interface = $config['interface']
template_filename="#{server}-#{interface}.tmpl"

template=''
begin
  template = Embedded_File.open(template_filename) { |x| x.read }
rescue Errno::ENOENT
  die("Template for server #{server} and interface #{interface} not found") 
end

config_data = apply_template(template, $config)

File.open($config['server-config-file'], 'w') do |output|
  output.puts(config_data)
end

command = make_command_line(server_info.command_template)
if ($verbose)
  $stderr.puts config_data
  $stderr.puts("[[  #{command}  ]]")
end
#system(command)

__END__
END-OF-FILE lighttpd-cgi.tmpl
server.document-root = "<?:www-dir:?>"

server.port = <?:port:?>

mimetype.assign = (
  ".html" => "text/html", 
  ".txt" => "text/plain",
  ".jpg" => "image/jpeg",
  ".png" => "image/png" 
)

static-file.exclude-extensions = ( ".fcgi", ".php", ".rb", "~", ".inc" )
index-file.names = ( "index.html" )
server.modules = ( "mod_cgi" )

cgi.assign = ( ".rb" => "<?:ruby-path:?>" )

server.errorlog = "<?:log-file:?>"
debug.log-request-header   = "enable"
debug.log-response-header  = "enable"
debug.log-request-handling = "enable"
debug.log-file-not-found   = "enable"
END-OF-FILE
