#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

require 'socket'


port = 54321

server = UDPSocket.open
server.bind(nil, port)

a = server.recvfrom(8);
data = a[0].unpack('c5')
puts data.join(':')
