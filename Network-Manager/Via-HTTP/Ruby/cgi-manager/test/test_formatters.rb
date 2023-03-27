#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby
require 'pp'
my_dir = File.dirname(File.expand_path($0));
$: << File.expand_path("#{my_dir}/../..")

require 'cgi-manager/formatters'
require 'cgi-manager/allocators'
require 'cgi-manager/entities'

stream1 = Stream_Info.new('stream'    => 'prova',
			  'server'    => 'gigi.pino.com',
			  'bandwidth' => '0.25e6',
			  'profile_name' => 'basic',
			  'profile_data' => Hash.new)

stream2 = Stream_Info.new('stream'    => 'prova',
			  'server'    => 'gigi.pino.com',
			  'bandwidth' => '0.25e6',
			  'profile_name' => 'vandermonde',
			  'profile_data' => { 
			    'gf-size' => 16,
			    'redfac'  => 4 
			  } )

stream = stream2

addr1 = ICE_Address.new("1 1 UDP 213070612 10.0.0.1 8998 typ host")
addr2 = ICE_Address.new("2 1 UDP 167182912 192.0.2.1 45664 typ srflx")

addr3 = ICE_Address.new("1 1 UDP 71273191  10.0.10.1 5671 typ host")
addr4 = ICE_Address.new("2 1 UDP 16781381  192.0.15.21 32516 typ srflx")

addr5 = ICE_Address.new("1 1 UDP 32167131 10.0.15.1 6662 typ host")
addr6 = ICE_Address.new("2 1 UDP 11133245 192.0.127.15 56567 typ srflx")

addr7 = IP_Address.new("127.0.0.1 12345")

user1 = User_Info.new('username'   => 'luigi',
		      'upload_bw'  => '1.5e6',
		      'candidates' => [addr1, addr2],
		      'n_inputs'   => 5,
		      'n_channels' => 4,
		      'max_output_streams' => 6,
		      'peer_id'    => '12345',
		      'stream_id'  => stream.unique_id)

user2 = User_Info.new('username'   => 'pippo',
		      'upload_bw'  => '0.5e6',
		      'candidates' => [addr3, addr4],
		      'n_inputs'   => 3,
		      'n_channels' => 2,
		      'max_output_streams' => 2,
		      'peer_id'    => '56789',
		      'stream_id'  => stream.unique_id)

user3 = User_Info.new('username'   => 'pinco',
		      'upload_bw'  => '1e6',
		      'candidates' => [addr5, addr7],
		      'n_inputs'   => 2,
		      'n_channels' => 3,
		      'max_output_streams' => 4,
		      'peer_id'    => '23422',
		      'stream_id'  => stream.unique_id)

if false
  peer_list = [Peer.new(user2, [1]), Peer.new(user3, [1, 2])]
else
  allocator = Allocators::Simple.new
  peer_list = allocator.assign_peers(nil, stream, user1, [user2, user3])

  if peer_list == Allocators::No_Peers
    $stderr.puts "Cannot satisfy requirements"
    exit 1
  end
end


peer_data = Peer_List.new(stream, user1, peer_list)


fmt = Formatters::XML.new

data, type = fmt.make_config_data(peer_data)

puts type
puts data






