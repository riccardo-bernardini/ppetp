#!/usr/bin/env ruby

#
# Trivial MMEDIA module, useful for testing purposes. The "reduction"
# operated by this module is just polyphase sampling (with step 3).
# Each packet has the format 
#
#        <timestamp> <component> <char>
#
# where all the fields are integer in base 10.  For example, the string 
# "Hello" would be decomposed as
#
#       0 0 72   (?H=72)
#       0 1 101  (?e=101)
#       0 2 108  (?l=108)
#       1 0 108
#       1 1 111  (?o=111)
#

# Ruby standard libraries
require 'socket'
require 'thread'
require 'getoptlong'

# For connection to the internal DB
require 'internal_state'

# For command reception
require 'tcp_command_source'

Thread.abort_on_exception=true

#
# Struct used to represent a component with a given timestamp
#
Data_Packet = Struct.new(:time,  # Timestamp
			 :comp,  # Component number
			 :data)  # Actual data

class Output
  #
  # class Output is a simplified version of the "smart queues + 
  # output tasks" found in the "true" MMEDIA module.  In this case,
  # for the sake of simplicity, a component is a polyphase component
  # of the original sequence of bytes.
  #
  # An object of class Output receives data (of class Data_Packet) through 
  # method +<<+.  
  #
  def initialize(n_comp, output)
    @peers = Array.new(n_comp)
    @buffer = Hash.new
    @head_time = -1
    @n_comp = n_comp
    @output = output
  end

  def complete?(buffer, head)
    buffer.has_key?(head) and (not buffer[head].include?(nil))
  end

  #
  # Receive new data
  #
  def <<(new_data)
    # Ignore data which belongs to the past
    return if new_data.time < @head_time

    # If @head_time < 0, this is the first time we are called
    @head_time = new_data.time if (@head_time < 0)

    # Make room for the data
    @buffer[new_data.time] ||= Array.new(@n_comp)

    @buffer[new_data.time][new_data.comp] = new_data.data

    while complete?(@buffer, @head_time)
      #
      # Here packet with timestamp @head_time is complete
      #

      # Output complete packet
      @output << @buffer[@head_time].join
      @output.flush
      # Send data to peers
      @peers.each_with_index do |peer, idx|
	next if peer.nil?
	host, port = peer
	packet = "#{@head_time} #{idx} #{@buffer[@head_time][idx]}"
	
	UDPSocket.open.send(packet, 0, host, port)
      end

      # Remove the processed packet and advance time
      @buffer.delete(@head_time)
      @head_time += 1
    end
  end

  #
  # Send component # stream to the peers waiting at port 
  # PORT of HOST
  #
  def send_to(stream, host, port)
    # This should never happen
    return false if stream > @n_comp

    # HOST and PORTH must be both nil or both non-nil
    return false unless (host.nil? == port.nil?)

    if (host.nil?)
      # Close stream # STREAM
      @peers[stream] = nil
    else
      #sock = UDPSocket.new
      p [stream, host, port]
      #sock.connect(host, port)
      @peers[stream] = [host, port]
      p @peers
    end

    return true
  end
end

#
# Start the thread which receives the data from the 
# other peers
#
def start_data_receiver(data_port, queue)
  Thread.new do
    # Open the UDP port
    server = UDPSocket.new
    server.bind(nil, data_port)

    loop do
      # Get next packet...
      packet, info = server.recvfrom(128)

      time, comp, data = packet.split

      # ...and write it to the queue
      queue << Data_Packet.new(time.to_i, comp.to_i, data.to_i.chr)
    end
  end
end

def process_command(command, parameters)
  case command.upcase
  when 'HELO'
    return true
  when 'QUIT'
    exit
  when 'SEND'
    case parameters.size 
    when 4
      timestamp, stream, address, crc = parameters
      host, port = address.upcase.split('U')
    when 3
      timestamp, stream, crc = parameters
      host = port = nil
    else
      return false
    end

    # p [stream, host, port]
    return Uscita.send_to(stream.to_i, host, port.to_i(16))
  else
    return false
  end
end

####################
### MAIN PROGRAM ###
####################

##----------------##
## Initialization ##
##----------------##

#
# My own ports
#
internal_port = 3333
data_port     = 3334
db_port       = nil
#
# Command line parsing
#
options = GetoptLong.new(["--server", "-S", GetoptLong::REQUIRED_ARGUMENT])

options.each do |opt, arg|
  case opt
  when "--server"
    db_port = arg.to_i
  else
    $stderr.puts "Unknown option #{opt}"
  end
end

raise "port number needed" unless db_port

#
# Create the data destination
#
Uscita = Output.new(3, $stdout)

#
# Create the command queue and start the TCP server.  The received 
# commands will be converted in Request objects and written to
# the command_queue.
#
input_queue = Queue.new
TCP_Source = TCP_Command_Source.new(internal_port, input_queue)


#
# Start the thread which listens to the UDP data port.  Each time a new
# data packet is received, the thread will write its information in 
# a Data_Packet object which will be written to the input_queue.
#
start_data_receiver(data_port, input_queue)

#
# Open the connection to the DB and write your info
#
db_connection = Internal_State::Connection.new(db_port)
db_connection['MMEDIA.INTERNAL_PORT']  = internal_port;
db_connection['MMEDIA.DATA_PORT']      = data_port;

#
# Get the port of the P2P module
#
# p2p_port = db_connection["P2P.INTERNAL_PORT"]


#
# Now enter in the never ending command loop
#
loop do
  packet = input_queue.pop

  case packet
  when Data_Packet
    Uscita << packet
  when Request
    com, *parameters = packet.command.split

    ok = process_command(com, parameters)

    if (ok) 
      TCP_Source.reply_to(packet, "OK");
    else
      TCP_Source.reply_to(packet, "ERR");
    end
  else
    $stderr.puts "Unknown class #{packet.class}"
  end
end

