module Unique_ID
  def Unique_ID::make_id(*args)
    return args.map{|str| "#{str.length}:#{str}"}.join('')
  end
end


class Peer_List 
  def initialize(stream, user, peers)
    @stream = stream
    @user   = user
    @peers  = peers
  end
  
  attr_reader :stream
  attr_reader :user
  attr_reader :peers
end


class Peer_Address
  class Invalid_Description < Exception
  end

  attr_reader :address
  attr_reader :port
end

class IP_Address < Peer_Address
  def initialize(config_string)
    entries = config_string.split
    raise Invalid_Description if entries.length != 2
    @address, @port = entries

    raise Invalid_Description if not port =~ /^[0-9]{1,5}$/
  end
end

class ICE_Address < Peer_Address

  class Unrecognized_Format < Exception
    def initialize(what)
      @format = what
    end

    attr_reader :format
  end


  def initialize(config_string, format='ice')
    case format.downcase
    when 'ice'
      parse_ice_string(config_string)
    else
      raise Unrecognized_Format(format)
    end

    raise Invalid_Description if not attribute_values_ok?
  end


  attr_reader :foundation    
  attr_reader :component_id  
  attr_reader :transport     
  attr_reader :priority      
  attr_reader :candidate_type   
  attr_reader :rel_addr       
  attr_reader :rel_port       
  attr_reader :extended_attr       

  private

  #
  # If the next string in entries is attr_name, return the 
  # successive string entries[1] and shift the two values away;
  # otherwise return nil
  #
  def get_attribute_maybe(entries, attr_name)
    if entries[0] == attr_name
      entries.shift
      return entries.shift
    else
      return nil
    end
  end

  def parse_ice_string(ice_string)
    entries = ice_string.split
    if (entries.size < 8 || ((entries.size-8) % 2 != 0))
      raise "Invalide ICE string"
    end

    @foundation      = entries.shift
    @component_id    = entries.shift
    @transport       = entries.shift
    @priority        = entries.shift
    @address         = entries.shift
    @port            = entries.shift

    @candidate_type  = get_attribute_maybe(entries, "typ")
    raise Invalid_Description if @candidate_type.nil?

    @rel_addr        = get_attribute_maybe(entries, "raddr")
    @rel_port        = get_attribute_maybe(entries, "rport")

    @extended_attr = Hash.new

    while not entries.empty? 
      attr_name  = entries.shift
      attr_value = entries.shift
      @extended_attr[attr_name] = attr_value
    end
  end

  @@attribute_syntax = {
    'foundation'      => %r([a-zA-Z0-9+/]{1,32}),
    'component_id'    => /[0-9]{1,5}/,
    'transport'       => ["UDP"],
    'priority'        => /[0-9]{1,10}/,
    'candidate_type'  => ["host", "srflx", "prflx", "relay"]
  }

  def attribute_values_ok?
    @@attribute_syntax.each do |attr_name, condition|
      value = instance_variable_get('@' + attr_name)
      return false if value.nil?

      ok = case condition
	   when Regexp
	     value =~ condition 
	   when Array
	     condition.include?(value)
	   else
	     raise "Internal error"
	   end

      return false if not ok
    end

    return true
  end
end
  
class Stream_Info
  @@mandatory_attr = %w(stream server bandwidth profile_name profile_data)
  @@optional_attr  = []

  def Stream_Info::add_mandatory_attribute(name)
    raise "bingo" if not name.is_a?(String)
    @@mandatory_attr << name
  end

  def Stream_Info::add_optional_attribute(name)
    raise "bingo" if not name.is_a?(String)
    @@optional_attr << name
  end

  def initialize(attr_table)
    missing=[]
    @@mandatory_attr.each do |name|
      if not attr_table.has_key?(name)
	missing << name
      else
	instance_variable_set("@#{name}", attr_table[name])
      end
    end

    if not missing.empty?
      raise "Missing mandatory attribute(s) '#{missing.join(', ')}'" 
    end

    @@optional_attr.each do |name|
      instance_variable_set("@#{name}", attr_table[name])
    end

    # @@mandatory_attr.each do |name|
    #   raise "bingo bongo" if not attr_table.has_key?(name)
    # end
    # 
    # @stream       = attr_table['stream']
    # @server       = attr_table['server']
    # @bandwidth    = attr_table['bandwidth'].to_f
    # @profile_name = attr_table['profile_name'].downcase
    # @profile_data = attr_table['profile_data']

    @bandwidth    = @bandwidth.to_f
    @profile_name = @profile_name.downcase

    @upload_quantum = case @profile_name
		      when 'basic'
			@bandwidth
		      when 'vandermonde'
			reduction = @profile_data['reduction_factor'].to_i
			@bandwidth / reduction
		      else
			raise "Unknown profile '#{@profile_name}'"
		      end

    @unique_id    = Stream_Info::make_id(@server, @stream)
  end

  def Stream_Info::make_id(arg1, arg2=nil)
    if (arg2.nil?)
      return Unique_ID::make_id(arg1['server'], arg1['stream'])
    else
      return Unique_ID::make_id(arg1, arg2)
    end
  end

  attr_reader :stream                # stream name
  attr_reader :server                # server name
  attr_reader :bandwidth             # required bandwidth
  attr_reader :profile_name          # profile used
  attr_reader :profile_data          # Hash with profile parameters
  attr_reader :upload_quantum        # bandwidth of a reduced stream
  attr_reader :unique_id             # ID of this stream
end

class User_Info
  @@mandatory_attr = %w(username   upload_bw     address_type  
                        address    stream_id     max_output_streams
                        n_inputs   n_channels    peer_id)

  @@optional_attr  = []

  def User_Info::add_mandatory_attribute(name)
    raise "bingo" if not name.is_a?(String)
    @@mandatory_attr << name
  end

  def User_Info::add_optional_attribute(name)
    raise "bingo" if not name.is_a?(String)
    @@optional_attr << name
  end


  def initialize(attr_table)
    missing=[]
    @@mandatory_attr.each do |name|
      if not attr_table.has_key?(name)
	missing << name
      else
	instance_variable_set("@#{name}", attr_table[name])
      end
    end

    if not missing.empty?
      raise "Missing mandatory attribute(s) '#{missing.join(', ')}'" 
    end

    @@optional_attr.each do |name|
      instance_variable_set("@#{name}", attr_table[name])
    end

	@address = [@address] unless @address.is_a? Array
    @candidates = @address.map do 
      |addr|
      case @address_type
      when "ip"
	IP_Address.new(addr)
      when "ice"
	ICE_Address.new(addr)
      else
	raise Processor::Bad_Command
      end
    end

    @upload_bw          = @upload_bw.to_f
    @n_inputs           = @n_inputs.to_i
    @n_channels         = @n_channels.to_i
    


    # @username           = attr_table['username']
    # @upload_bw          = attr_table['upload_bw'].to_f
    # @n_inputs           = attr_table['n_inputs'].to_i
    # @n_channels         = attr_table['n_channels'].to_i
    # @peer_id            = attr_table['peer_id']
    # @candidates         = attr_table['candidates']
    # @max_output_streams = attr_table['max_output_streams']
    # @stream_id          = attr_table['stream_id']

    @upper_peers = Array.new
    @lower_peers = Array.new

    @unique_id   = User_Info::make_id(@stream_id, @username)
  end

  def User_Info::make_id(arg1, arg2=nil)
    if arg2.nil?
      return Unique_ID::make_id(Stream_Info::make_id(arg1), arg1['username'])
    else
      return Unique_ID::make_id(arg1, arg2)
    end
  end


  def add_upper_peer(peer)
    @upper_peers << peer
  end

  def remove_upper_peer(peer)
    @upper_peers.delete(peer)
  end

  def add_lower_peer(peer)
    @lower_peers << peer
  end

  def remove_lower_peer(peer)
    @lower_peers.delete(peer)
  end

  def available_streams
    return @max_output_streams - @lower_peers.size
  end

  def set_variable(name, value)
    @variables[name]=value
  end

  def get_variable(name)
    return @variables[name]
  end

  def attribute(name)
    var = "@#{name}"
    raise "Undefined attribute #{name}" unless instance_variables.include?(var)
    return instance_variable_get(var)
  end


  attr_reader :unique_id            # Unique ID used internally

  attr_reader :username             # Name of the user
  attr_reader :upload_bw            # Upload bandwidth
  attr_reader :candidates           # User address(es)
  attr_reader :n_inputs             # N. of requested input streams
  attr_reader :n_channels           # N. of output channels
  attr_reader :max_output_streams   # Max n. of output streams
  attr_reader :peer_id              # PPETP ID (given to other peers)
  attr_reader :stream_id            # Internal ID of the associate stream

  attr_reader :upper_peers          # Array of the Unique_ID of the upper_peers
  attr_reader :lower_peers          # Array of the Unique_ID of the lower_peers
end

Peer = Struct.new("Peer", "user", "requested_channels")
