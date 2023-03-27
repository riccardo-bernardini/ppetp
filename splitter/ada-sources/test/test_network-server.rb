#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

require 'socket'
require 'thread'

def start_child
  Thread.new do
    sleep 2
    $child  = IO.popen('./test_network')
  end
end

def start_server
  return TCPServer.new(4242)
end

def basic_TCP_test(server_socket)
  session = server_socket.accept
  command = session.gets
  a, b = command.split

  session.print "#{a.to_i+b.to_i}\r\n"
  session.close
end

def basic_UDP_test(server_socket)
  session = server_socket.accept
  in_port, out_port = session.gets.split.map {|x| x.to_i}

  session.print "#{in_port*2}\r\n"
  session.close
  sock = UDPSocket.open
  sock.bind(nil, in_port)
  data, ignored = sock.recvfrom(2)

  UDPSocket.open.send(data.reverse, 0, 'localhost', out_port);
  $stderr.puts("ricevuto #{data}");
end

### 
### MAIN
### 

server_socket = start_server
start_child
basic_TCP_test(server_socket)
basic_UDP_test(server_socket)




puts $child.read



