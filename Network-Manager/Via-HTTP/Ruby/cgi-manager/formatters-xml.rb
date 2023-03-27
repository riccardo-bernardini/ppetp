require 'rexml/document'

module Formatters
  class XML
    Formatters::register('xml', XML)

    def initialize
    end

    MIME_Type = "text/ppetp-config-xml"

    def make_config_data(peer_data)
      stream_info = peer_data.stream
      user_info   = peer_data.user
      upper_peers = peer_data.peers
      
      doc = REXML::Document.new("<configuration/>")
      doc.root.add(make_session_node(stream_info, user_info, upper_peers))

      buffer = ""
      doc.write(buffer, 1)
      return [buffer, Formatters::XML::MIME_Type]
    end

    private

    def make_session_node(stream_info, user_info, upper_peers)
      result = REXML::Element.new("session")
		
		tmp = rand(2**12)
		while tmp == 0
			tmp = rand(2**12)
		end

      result.add_attribute("streamID", tmp.to_s)
      result.add(make_profile_node(stream_info))
      result.add(make_output_node(stream_info, user_info))
      result.add(make_input_node(stream_info, upper_peers))
      
      return result
    end

    def make_profile_node(stream_info)
      result = REXML::Element.new("profile")
      result.add_attribute("name", stream_info.profile_name)
      stream_info.profile_data.each do 
	|key, value|

	result.add(make_parameter_node(key, value))
      end

      return result
    end

    def make_parameter_node(name, value)
      result = REXML::Element.new("parameter")
      result.add_attribute("name", name)
      result.add_attribute("value", value)

      return result
    end

    def make_output_node(stream_info, user_info)
      result = REXML::Element.new("output")
      (0...user_info.n_channels).each do |channel| 
	result.add(make_channel_node(stream_info, user_info, channel))
      end

      return result
    end

    def make_channel_node(stream_info, user_info, channel)
      result = REXML::Element.new("channel")
      result.add_attribute("id", channel.to_s)
      return result
    end

    def make_input_node(stream_info, upper_peers)
      result = REXML::Element.new("input")
      upper_peers.each do |peer|
	result.add(make_peer_node(stream_info, peer))
      end

      return result
    end

    def make_peer_node(stream_info, peer)
      result = REXML::Element.new("peer")
      result.add_attribute('id', peer.user.peer_id)
      result.add_attribute('address-type', case peer.user.candidates[0]
					   when IP_Address
					     "ip"
					   when ICE_Address
					     "ice"
					   else
					     raise "Unknown address type #{peer.user.candidates[0].class}"
					   end)


      peer.user.candidates.each do |candidate|
	result.add(make_address_node(candidate))
      end

      peer.requested_channels.each do |channel|
	result.add(make_remote_channel_node(channel))
      end

      return result
    end

    def make_remote_channel_node(channel)
      result = REXML::Element.new("channel")
      result.add_attribute("id", channel.to_s)

      return result
    end

    def make_address_node(candidate)

      result = REXML::Element.new("address")
      result.add_attribute("address", candidate.address)
      result.add_attribute("port", candidate.port)

      case candidate
      when IP_Address
	# null
      when ICE_Address
	add_ice_parameters(result, candidate);
      else
	raise "Unknown address type #{candidate.class}"
      end

      return result
    end

    def add_ice_parameters(target, candidate)
      mandatory_param = %w(foundation component_id transport priority candidate_type)
      optional_param  = %w(rel_addr rel_port)

      mandatory_param.each do |param_name|
	value = candidate.send(param_name)
	target.add(make_parameter_node(param_name, value.to_s))
      end

      optional_param.each do |param_name|
	value = candidate.send(param_name)
	if (value)
	  target.add(make_parameter_node(param_name, value.to_s))
	end
      end

      candidate.extended_attr.each do |name, value|
	target.add(make_parameter_node(name, value.to_s))
      end
    end
  end
end
