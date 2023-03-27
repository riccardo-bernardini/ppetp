#!/usr/bin/env ruby

require 'socket'

to_tunnel   = 8000
from_tunnel = 9000

p_loss = ARGV.shift || '0'
n_trial = 1000
ricevuti = Array.new
trasmessi = Array.new

child = fork do 
  exec("ruby ./udp_tunnel.rb #{to_tunnel} #{from_tunnel} #{p_loss}")
end

at_exit {
  Process.kill("SIGKILL", child)
  Process.wait
}


lettore = Thread.new do
  input = UDPSocket.open
  input.bind(nil, from_tunnel)

  while true
    ricevuti << input.recvfrom(32)
  end
end

lettore.abort_on_exception=true

n_trial.times do |k|
  packet = sprintf("%04d:%020x", k, rand(1e6));
  trasmessi << packet
  UDPSocket.open.send(packet, 0, 'localhost', to_tunnel);
  sleep(0.05)
end

puts "End of transmission... sleeping"
sleep 5

ricevuti = ricevuti.map {|x| x[0]}

s = "#{ricevuti.length}/#{trasmessi.length}"
puts "P_loss = #{1.0-ricevuti.length.to_f/trasmessi.length} (#{s})"

flags = Array.new(trasmessi.length, "0");

wrong=[]
ricevuti.each do |packet|
  idx, text = packet.split(':')
  idx = idx.to_i

  wrong << idx if (packet != trasmessi[idx])
  flags[idx]="1"
end

puts flags.join

puts "# errors = #{wrong.length}/#{trasmessi.length}"

