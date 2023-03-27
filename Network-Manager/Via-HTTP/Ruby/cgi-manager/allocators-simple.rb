module Allocators
  class Simple
    Allocators::register('simple', Simple)

    def initialize
    end

    Resource = Struct.new("Resource", "unique_id", "n_channels")

    def assign_peers(stream_db, stream_info, new_user, peer_list)
      id_to_user = Hash.new
      peer_list.each { |user| id_to_user[user.unique_id] = user }

      #
      # First find how many channels we can ask from each peer
      # The number of possible channels must be less or equal than 
      # the number of node channels and the number of available
      # streams
      #
      availability_list = get_availability_list(peer_list)
      
      upper_peers = allocate_channels(availability_list, new_user.n_inputs)

      return Allocators::No_Peers if upper_peers == Allocators::No_Peers

      result = Array.new
      upper_peers.each do 
	|peer_id, channels|

	result << Peer.new(id_to_user[peer_id], channels)
      end

      return result
    end

    private

    #
    # Return an array of Resource (pairs (unique_id,  n_channels)) 
    # sorted in decreasing order by the number of the channels
    # that can be required to the node.  Note that such a number
    # cannot be larger than the minimum between the number of
    # channels produced by the node and the number of still
    # available streams
    #
    def get_availability_list(peer_list)
      result = peer_list.map do 
	|peer| 
	raise "Internal error" if peer.available_streams < 0

	usable_channels = [peer.available_streams, peer.n_channels].min
	Resource.new(peer.unique_id, usable_channels)
      end

      return result.sort { |x, y| y.n_channels <=> x.n_channels }
    end

    #
    # Return an Hash that maps Unique_ID to the set of channels required
    # to the corresponding peer.  Return Allocators::No_Peers if not
    # enough peer are available
    #
    def allocate_channels(availability_list, needed_channels)
      raise "Internal error" if availability_list.any? {|x| x.n_channels < 0}

      current_channel = 0
      result = Hash.new

      while needed_channels > 0
	availability_list.delete_if { |x| x.n_channels < 1 }

	return Allocators::No_Peers if availability_list.empty?

	n_peers = [availability_list.length, needed_channels].min

	n_peers.times do |idx|
	  peer_id = availability_list[idx].unique_id

	  if result.has_key?(peer_id)
	    result[peer_id] << current_channel
	  else
	    result[peer_id] = [current_channel]
	  end

	  availability_list[idx].n_channels -= 1
	end

	needed_channels -= n_peers
	current_channel += 1
      end

      return result
    end
  end
end
