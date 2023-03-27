#!/usr/bin/env ruby

#
#
#                       --- USER-LEVEL DOC ---
#
# ===================
# == What is this? ==
# ===================
# 
# -- In brief
#
# This is an "RTP-splitter", that is a piece of software that dispatch
# RTP packets to different players on the basis of the SSRC field (see
# RFC 3550 for details about the SSRC field, here it suffices to say
# that it is a field of the RTP header that identifies the source of the
# packet).  
#
# -- Detailed description
# 
# More precisely, the RTP-splitter has the following behaviour
# 
#   1) Listen on the UDP port specified by the user
# 
#   2) When an RTP packet arrives
# 
#       2.1) If the SSRC of the packet is "new" (that is, this is the
#            first packet with that SSRC)
# 
#              2.1.a) Start a new player and ask it to reproduce the
#                     multimedia stream that will receive on  a
#                     localhost (127.0.0.1) UDP port
# 
#       2.2) If the SSRC is not new, send the packet to the
#            corresponding player
# 
#   3) If after T seconds no more packets with a given SSRC are
#      received, close the corresponding player
# 
# ======================
# == How do I use it? ==
# ======================
# 
# Just run
# 
#   rtp_splitter.rb [--input-port=port] [--timeout=t] [--player=player]
# 
# 
#                       --- INTERNAL DETAILS ---
#
# An RTP-splitter has two major duties
#
#   1) Receive RTP packets from an UDP port and dispatch them (with small 
#      latency) to other ports
#
#   2) Start (and kill) when needed new players
#
# Since each available player has its own control interface, the second 
# job would require some scripting language (e.g., Ruby) since this would
# allow to add new "player controllers" easily. (If some form of dynamic
# discover is implemented, even a user could be able to add its own 
# player controller).  However, the first job require efficiency, so
# it should be implemented in some compiled language.
#
# The chosen solution is to write a Ruby "main program" that will start 
# a "true dispatcher" written in Ada.  In order to start & kill the 
# external players the Ada program will need to communicate with the Ruby 
# program.  This will be done via a local TCP connection (to 127.0.0.1).  
# We choose to use local TCP connections instead of other means because
# (almost?) all currently available systems support TCP.
#

$my_dir = File.dirname(File.expand_path($0));
$: << File.join($my_dir, 'ruby-lib')


$running_processes = Array.new

require 'socket'
require "timeout"

require 'getoptlong-ext'
require 'configuration_tables'
require 'embedded_file'

require 'rtp_splitter-players'

class Player_Table
  #
  # A player table is a table that associate "players" (any Ruby
  # object) "player_id" (integer numbers).  
  #
  # The main methods of a Player_Table are
  #   add:: add(player) adds a new player to the table and returns
  #         the corresponding ID
  #   get:: get(id) gets the player associated to id or nil 
  #         if not found
  #   remove:: remove(id) remove the player associated to id from 
  #            the table (a NOP if no such player exists)
  #
  def initialize
    @next_player_id = 1
    @id_to_player = Hash.new
  end

  def add(player)
    this_id = @next_player_id
    @id_to_player[this_id] = player
    @next_player_id += 1
    return this_id
  end

  def get(player_id)
    @id_to_player[player_id.to_i]
  end

  def remove(player_id)
    @id_to_player.delete(player_id.to_i)
  end
end

# ----------------------------
# -- PROGRAM INITIALIZATION --
# ----------------------------

def die(msg)
  $stderr.puts(msg)
  exit 1
end

def print_help_and_die
  die("Usage: rtp_splitter [options] port")
end

def get_config_data
  config_data = Configuration_Table.new('defaults' => {
					  'input-ports'  => [5004],
					  'timeout'      => 5.0,
					  'player'       => 'vlc',
					  'server-port'  => 42430,
					  'mirror'       => nil,
					  'net-address'  => nil,
					  'splitter-name' => 
					  "#{$my_dir}/ada-sources/exe/real_splitter",
					  'driver-class' => Generic_Player
					},
					'env-var'  => 'RTP_SPLITTER_CONFIG',
					'parsers'  => {
					  'input-ports' => '#,int',
					  'timeout'     => 'float',
					  'server-port' => 'int',
					  'net-address' => 'text'
					},
					'option-settable' => true)
  
  option_syntax = "Usage: #{File.basename($0)}  [options]

i | input-ports=text       // Comma-separated list of UDP ports
s | server-port=int        // Internal TCP port
n | net-address=text       // Address of the peer
p | player=path            // Player to be used
l | log-file=path          // File used for logging
T | timeout=float          // Timeouts in seconds
m | mirror=port,host:port  // Mirror localhost:port to host:port
"

  unparsed_options = config_data.parse_options(option_syntax)

  print_help_and_die(option_syntax) unless unparsed_options.empty?

  return config_data
end

# ----------------
# -- TCP SERVER --
# ----------------

# Start a TCP server that listens to port :port:. If the port is 
# busy, try up to n_trials randomly chosen ports.  If sucessful
# (a free port is found) it returns a vector with the server socket 
# and the corresponding port; if unsuccesful it raises Errno::EADDRINUSE.
def start_tcp_server(port, n_trials=100)
  count = 0
  begin
    server = TCPServer.new(port)
  rescue Errno::EADDRINUSE
    die("Too many busy ports") if count == n_trials
    port  += 1 + rand(7)
    count += 1
    retry
  end

  return [server, port]
end

# This function is called in a new thread when a new connection
# to the TCP server is received.  It runs in a never ending loop
# reading requests, processing them and returning replies
def sub_server(session)
  player_table = Player_Table.new
  again = true

  while again
    $stderr.puts "in ascolto..."
    request = session.gets("\r\n")
    $stderr.puts "Received: [#{request}]"
    reply = serve_request(request, player_table)
    $stderr.puts "Replying [#{reply}]"
    session.print "#{reply}\r\n"
  end
end

# This function processes a request received from a TCP client
# It returns a reply to be sent to the client
def serve_request(request, player_table)
  command, *param = request.split
  
#	p "Son qui!"

  case command.downcase
  when 'start'
    return "410 1-2 parameters expected" unless param.size >= 1
    port = param[0]
    host = (param[1] || "127.0.0.1")

    return "410 wrong port syntax for '#{port}'" unless port =~ /^[0-9]+$/

	 p "Vlc started on" + host + " " + port

    player_class = Player_Root_Class.handler_for($config['player'])
    player = player_class.new($config['player'], port, host)

    return "500 could not run player" unless player.running?

    player_id = player_table.add(player)
	 pid = player.pid
	 $stderr.puts("Avviato player #{pid}")
	 $running_processes << pid

	 sleep 3

    return "0 #{player_id}"
  when 'kill'
    return "410 1 parameter expected" unless param.size == 1
    player_id = param[0]
    player = player_table.get(player_id)
    return "410 no such player '#{player_id}'" if player.nil?

    pid = player.pid
	 $stderr.puts("scaduto player #{pid}")
    
 
 	 ok, reason = player.die
	
	 p "500 could not kill player '#{player_id}': #{reason}" unless ok

    return "500 could not kill player '#{player_id}': #{reason}" unless ok


    player_table.remove(player_id)
    
    return "0"
  else
    return "410 Unrecognized command '#{command}'"
  end

  raise "I should not arrive here!"
end

# -------------------------
# -- SPLITTER SUBPROCESS --
# -------------------------

#
# The actual RTP splitting work is done by an external Ada program.
# Since the Ada splitter handles just one media stream (such as
# audio, video, slides), in order to handle multiple media streams
# at the same time, we need to start multiple splitters.
#

# Start the Ada program that does the actual splitting and return the
# PID of the corresponding child process.  This function will be called
# multiple times in order to start all the needed Ada splitters
def start_real_splitter(udp_input_port, my_port)

  net_address = $config['net-address']
  splitter_parameters =["src=#{udp_input_port}", "ctl=#{my_port}", "address=#{net_address}",
	"timeout=#{$config['timeout']}"]
  if $config['mirror']
    splitter_parameters << "target=#{$config['mirror']}"
  end

  p splitter_parameters 


  splitter_path = $config['splitter-name']

  my_dir = File.expand_path(File.dirname($0))
  if splitter_path[0..0] != '/'
    splitter_path = File.join(my_dir, splitter_name)
  end

  die("#{splitter_path} does not exist")    unless File.exist?(splitter_path)
  die("#{splitter_path} is a directory")    unless File.file?(splitter_path) 
  die("Could not execute #{splitter_path}") unless 
    File.readable?(splitter_path) && File.executable?(splitter_path) 

#  p [splitter_path] + splitter_parameters
  splitter_pid = fork do
    $stderr.puts
	 $stderr.puts splitter_path + ' ' + splitter_parameters.join(' ')
    $stderr.puts
    # I am the child
    #exec(splitter_path, "src=#{udp_input_port}", "ctl=#{my_port}")
    exec ( splitter_path + ' ' +  splitter_parameters.join(' ') )

    # never get here
  end

  p splitter_pid
    
  return splitter_pid
end

# Start many copies of the Ada splitter, one for every media
# stream we expect in input.  my_port is the port of the internal 
# TCP server, input_ports are the UDP ports where the streams
# are expected.
def create_splitter_sub_processes(my_port, input_ports)
  splitter_pids = []
  input_ports.each do |source_port|
    splitter_pid = start_real_splitter(source_port, my_port)
    splitter_pids << splitter_pid

    $stderr.puts "Splitter PID:        #{splitter_pid}"
    $stderr.puts "Reads from port:     UDP/#{source_port}"
  end

  return splitter_pids
end



########################
###   MAIN PROGRAM   ###
########################

# -- Inizialization 

$config = get_config_data

if $config['log-file'] 
  begin
    file = File.open($config['log-file'], 'a')
    file.sync = true
  rescue
    $stderr.puts "Warning: cannot open log file #{$config['log-file']}"
    $stderr.puts "Logging to standard error"
    file = nil
  end

  $stderr=file unless file.nil?
end

# -- Start the internal TCP server

server, my_port = start_tcp_server($config['server-port'])

$stderr.puts "Control port:        TCP/#{my_port}"

# -- Start the splitters

splitter_pids = create_splitter_sub_processes(my_port, $config['input-ports'])


# -- Look if any players are closed by the user

th = Thread.new {

	while true
		# look if this pid is running
		$running_processes.each{ |pid|
			

			begin
				res = Process.wait(pid, Process::WNOHANG)
# --			$stderr.puts("pid #{pid} : status : #{$?}")

				if $? != nil
					if $?.exited?
						$stderr.puts("Stopped by the user")
						$running_processes.delete(pid)
						
						# KILL ALL
						kill_path = File.join($my_dir, '../clear.sh')
						$stderr.puts(kill_path)
						exec(kill_path)
						exit 1
					end	
				end
			rescue
				$stderr.puts("Closed because stream finished: #{pid}")
				$running_processes.delete(pid)
			end
		}

		sleep 1
	end
}


# -- Wait for connections and starts sub servers to handle them
threads = []
while true
  session = server.accept
  new_thread = Thread.new(session) { |session| sub_server(session) }
  new_thread.abort_on_exception = true
  threads << new_thread
end
