#!/usr/bin/env ruby

require 'socket'
require 'thread'
require 'English'

require 'internal_state'
require 'tcp_command_source'
require 'external_module'
require 'format_command'
require 'graphical_interface'
require 'LDS_stream'


at_exit { do_exit_cleanup }

Thread.abort_on_exception=true

# MbO 
# MbO class Timestamp
# MbO   @timestamp=-1
# MbO   def Timestamp.next
# MbO     @timestamp += 1
# MbO   end
# MbO end

def p2p_auth_callback(request, param)
  print "Login: "
  login=gets
  print "Password: "
  passwd=gets

  TCP_Interface.reply_to(request, ['AUTH', login, passwd])
end

def gui_open_callback(request, param)
  url = param[0]
  puts "To P2P: 'PLAY #{url}'"

  P2P_Module << Command_Formatter.format_command("PLAY", url, :complete)
end

def do_exit_cleanup
  P2P_Module.quit
  MMedia_Module.quit
  Player_Module.quit
end

def gui_quit_callback(request, param)
  do_exit_cleanup
  exit
end

# $config = { 
#   'P2P-bin'     => '%~/P2P/Udine/CLient/obj/Create_Client',
#   'MMEDIA-bin'  => 'ruby DSP/Trivial/trivial_mmedia.rb --server %d',
#   'PLAYER-bin'  => '%vlc-adapter.rb',
#   'my-port'     =>  54329,
#   'state-port'  =>  54328,
# }

#
# Create the queue used to receive the commands
#  
Commands = Queue.new

#
# Create the graphical interface
#
GUI = Graphical_Interface.new(Commands)


#
# Start the TCP server
#
TCP_Interface = TCP_Command_Source.new($config['my-port'], Commands)


#
# Start the internal DB
#
State_Server = Internal_State::Server.new

#
# Write your variables to the DB
#
State_Server['UI.WINDOW_ID'] = GUI.viewport

port_range_size = 1000
my_port = $config['my-port'].to_i

State_Server['UI_PORT'] = my_port
State_Server['MAIN.PORT_RANGE_SIZE']   = port_range_size

current_port = my_port + port_range_size
['MMEDIA', 'P2P', 'PLAYER'].each do |name|
  State_Server["MAIN.#{name}_FIRST_PORT"] = current_port
  current_first += port_range_size
end

State_Server.listen_to_port($config['state-port'])

#
# Start the subprocesses
#
MMedia_Module = External_Module.new($config, 'MMEDIA');
P2P_Module    = External_Module.new($config, 'P2P');
Player_Module = External_Module.new($config, 'PLAYER');


#
# Start the thread which will read from queue Command the 
# commands received by other modules and graphical interface. 
# Each command has a callback function whose name is the command
# name with any '.' replaced by '_' and with "_callback" appended to 
# the end (e.g. the default callback of "QUIT" is quit_callback).  The 
# default can be overriden by writing the callback name in Hash table
# callback_table.
#
callback_table = Hash.new

Thread.new do 
  while true
    request = Commands.pop
    command, *params = request.command.split
    command = command.downcase

    callback_name = callback_table[command] || 
      "#{command.tr('.', '_')}_callback"
    
    send(callback_name, request, params)
  end
end

#
# Well, that's all.  Just start the GUI main loop.
#

GUI.mainloop
