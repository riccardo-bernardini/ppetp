#!/usr/bin/env ruby

#
# This mini script reads packets on an UDP port and copy it to 
# another UDP port.  The two port numbers are given as parameters.
# If a third parameter is present, it is interpreted as a loss
# probability.
#
# This script was written to test the PPMTP software.
#

require 'socket'

MAX_PACKET_SIZE = 4096

if (ARGV.length < 2)
  puts "Usage: #{$0} port-in port-out [p_loss]"
  exit 1
end

port_in, port_out, p_loss  = ARGV
p_loss = p_loss.to_f unless p_loss.nil?

input = UDPSocket.open
input.bind(nil, port_in)

if (p_loss.nil?)
  while true
    packet = input.recvfrom(MAX_PACKET_SIZE)    
    UDPSocket.open.send(packet[0], 0, 'localhost', port_out)
  end
else
  while true
    packet = input.recvfrom(MAX_PACKET_SIZE)    
    UDPSocket.open.send(packet[0], 0, 'localhost', port_out) if (rand > p_loss)
  end
end


