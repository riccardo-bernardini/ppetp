#
# This file define the "Processor" class.  
#
# --------------------------
# -- What is a processor? --
# --------------------------
#
# The duty of a Processor is to process the received HTTP query.  It is 
# independent on the method used to send the request (GET or POST) and 
# also on the way the HTTP server interfaces with this script (CGI, 
# SCGI, FastCGI, whatever...)
#
# -----------------------------
# -- What does it answer to? --
# -----------------------------
#
# Beside the usual constructor, Processor exports only one 
# method: HANDLE_REQUEST.
#
#   * What does HANDLE_REQUEST expect in input?
#
#       It expects in inputs the query as "HTTP query", that is, a string 
#       similar to 
#
#         param1=foo&param2=pippo&param3=gigi
#
#   * What does HANDLE_REQUEST return?
#
#       It returns a Processor::Reply which is a struct that represents an 
#       HTTP reply (with fields err_code, message, headers and body) and 
#       that can be given "as it is"  to CGI.write
#
# -------------------------------
# -- What modules does it use? --
# -------------------------------
#
# A Processor to do its work it requires 
#
#    o A  STREAM_DB
#    o An ALLOCATOR
#
# to be given to the constructor.
#
#    * >> What is a STREAM_DB? <<
#
#        The duty of a STREAM_DB is to keep the database of the currently
#        opened streams and their users.  According to the "duck typing"
#        phylosophy of Ruby, a STREAM_DB is identified by its methods.
#        More precisely, a STREAM_DB is an object that responds to
#        the following methods
#
#          # add_stream(Stream_Info)
#          # Stream_Info = get_stream(Stream_ID)
#          # remove_stream(Stream_ID)
#
#          # Array = get_stream_users(Stream_ID)
#
#          # add_user(stream_id, User_Info)
#          # connect_peers(User_ID, User_ID)
#          # remove_user(User_ID)
#
#          # set_variable(name, value)
#          # get_variable(name)
#
#          # Reply = STREAM_DB.open { block }
#
#       All the methods should be clear, only the last one needs some
#       explanation.  Method #open "locks" the DB, execute block, "unlock"
#       the DB and return Reply or nil as a result. If nil is returned,
#       it means that the default HTTP OK reply with no body can be used,
#       otherwise the Reply is used.  What "lock" and "unlock" mean depend
#       on the actual DB implementation, they could even reduce to 
#       NOPs.
#
#    * >> What is an ALLOCATOR? <<
#
#        The duty of an ALLOCATOR is to find suitable peers for a given 
#        node.  An ALLOCATOR responds to the following methods
#
#          # assign_peers(stream_db, Stream_Info, User_Info, user_list)
#
#             The first parameter is the stream that the user wants
#             to join, the second parameter is the new user, the 
#             third parameter is a list of candidate peers.  The function
#             returns 
#
#               + if SUCCESSFULL, an array of Peer, where Peer is a 
#                 struct with a field "user" (User_Info) and an array of
#                 integers "requested_channels"
#
#               + if UNSUCCESSFULL, an exception of (sub)class 
#                 Allocators::Error.  Currently the only defined
#                 subclass is Allocators::No_Peers used when there are 
#                 not enough peers to satisfy the request
#
#

require 'cgi-manager/cgi_utilities'

class Processor

  class Bad_Command < Exception
  end

  def initialize(stream_db, allocator)
    @stream_db = stream_db
    @allocator = allocator
  end


  # --------------------
  # -- Handle_Request --
  # --------------------

  def handle_request(param)
    begin
      param = param.dup

      Common_Parameters.each do |name|
	raise Bad_Command if param[name].nil?
      end
    
      # p param; p param['command']
      handler = Request_Handlers[param['command']]
      case handler
      when nil
	raise Bad_Command 
      when ''
	handler = method(param['command'])
      end

      return @stream_db.open { handler.call(param) }
    rescue Exception => exception
      return exception
    end
  end

  private

  #
  # Hash table that maps request to request handler.  If the request
  # handler is the empty string, use the method of Processor::Handlers
  # with the same name of the request
  #
  Request_Handlers = { 
    "create_stream"  => '',
    "destroy_stream" => '',
    "join_user"      => '',
    "remove_user"    => ''
  }

  #
  # Parameters required by all the commands
  #
  Common_Parameters = [
    "command", "stream", "server"
  ]

  #
  # List of parameters required by profiles
  #
  Profile_Parameters = {
    'basic'       => [],
    'vandermonde' => ['gf_size', 'reduction_factor']
  }

  # -------------------
  # -- CREATE_STREAM --
  # -------------------
  
  def create_stream(param)
    check_parameters(param, "bandwidth", "profile_name")
#		puts param
    param["profile_data"] = get_profile_parameters(param)

    info = Stream_Info.new(param)

    @stream_db.add_stream(info)
    return nil
  end

  # --------------------
  # -- DESTROY_STREAM --
  # --------------------

  def destroy_stream(param)
    @stream_db.remove_stream(Stream_Info::make_id(param))
    return nil
  end

  # ---------------
  # -- JOIN_USER --
  # ---------------

  def join_user(param)
    check_parameters(param, 
		     "upload_bw", 
		     "username",
		     "address", 
		     "address_type", 
		     "n_inputs",
		     "n_channels")

    stream_id = Stream_Info::make_id(param)
    stream_info = @stream_db.get_stream(stream_id)

#	$stderr.puts "-----;#{param['upload_bw'].to_f};#{stream_info.upload_quantum};------"
    max_output = (param['upload_bw'].to_f / stream_info.upload_quantum).floor;

    param["max_output_streams"] = max_output
    param["stream_id"] = stream_id
    new_user = User_Info.new(param)

    user_list   = @stream_db.get_stream_users(stream_id)

    alloc_result = @allocator.assign_peers(@stream_db, stream_info, new_user, user_list)

    if (not Allocators::ok?(alloc_result))
      return alloc_result
    end

    @stream_db.add_user(stream_id, new_user)

    upper_peers = Allocators::peers(alloc_result)
    upper_peers.each do |peer|
      @stream_db.connect_peers(peer.user, new_user);
    end

    return Peer_List.new(stream_info.dup, new_user.dup, upper_peers)
  end
  
  # -----------------
  # -- REMOVE_USER --
  # -----------------

  def remove_user(param)
    check_parameters(param, "username")

    @stream_db.remove_user(User_Info::make_id(param))
    return nil
  end

  def check_parameters(param, *required)
    required.each do |name|
      raise "missing #{name}" if not param.has_key?(name)
    end

    # if (not required.include?("*"))
    #   all_required = required + Common_Parameters
    #   
    #   param.keys.each do |name|
    # 	raise "" if not all_required.include?(name)
    #   end
    # end
  end

  def get_profile_parameters(param)
    profile = param['profile_name']

	#	puts profile
    prof_param = Profile_Parameters[profile]
    raise nil if prof_param.nil?

    result = Hash.new
    prof_param.each do |name|
      true_name = "param-#{profile}-#{name}"
	#	$stderr.puts "==#{true_name}:#{param[true_name]}=="
      raise "" if not param.has_key?(true_name)
      result[name] = param[true_name]
    end

    return result
  end
end
