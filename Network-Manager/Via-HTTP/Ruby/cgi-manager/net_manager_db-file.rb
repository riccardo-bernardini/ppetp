require 'yaml'
require 'cgi-manager/cgi_utilities'
require 'pp'

module NetManager_DB
  class Marshalled_File
    NetManager_DB::register('file', Marshalled_File)
    DB = Struct.new("DB",
		    "streams",             # stream_id -> stream_info
		    "users",               # user_id   -> user_info
		    "stream_to_users",     # stream_id -> array of user_id
		    "variables")          


    def Marshalled_File.create_if_needed(filename, mode)
      handler = @@mode_handlers[mode]

      raise "Unimplemented mode '#{@filemode}'" if handler.nil?

      return if FileTest.exist?(filename)

      empty_db = DB.new(Hash.new, Hash.new, Hash.new, Hash.new)
      
      File.open(filename, 'w') do |db_file|
	db_file.flock(File::LOCK_EX)
	db_file.print(handler.dump.call(empty_db))
      end
    end

    class Mode_Handler
      def initialize(arg)
	case arg 
	when Module
	  @load = arg.method("load")
	  @dump = arg.method("dump")
	when Hash
	  @load = arg['load']
	  @dump = arg['dump']
	when Array
	  @load, @dump = arg
	else
	  raise "Internal error"
	end
      end

      attr_reader :load
      attr_reader :dump
    end

    @@mode_handlers = { 
      'text'     => Mode_Handler.new(YAML),
      'yaml'     => Mode_Handler.new(YAML),
      'marshal'  => Mode_Handler.new(Marshal),
      'binary'   => Mode_Handler.new(Marshal),
    }

    def initialize(param_string)
      parameters = CGI::parse(param_string)
      raise "Missing filename"   unless parameters.has_key?('filename')
      raise "Too many filenames" unless parameters['filename'].is_a?(String)

      @filename = parameters['filename']
      @filemode = (parameters['mode'] || 'text')
      @db = nil
    end

    # ----------
    # -- OPEN --
    # ----------

    def open
      handler = @@mode_handlers[@filemode];
      raise "Unimplemented mode '#{@filemode}'" if  handler.nil?


      Marshalled_File.create_if_needed(@filename, @filemode)
      result = nil
      begin
	File.open(@filename, 'r+') do |db_file|
	  db_file.flock(File::LOCK_EX)
	  @db = handler.load.call(db_file.read)
	  result = yield
	  db_file.truncate(0)
	  db_file.seek(IO::SEEK_SET, 0)
	  db_file.print(handler.dump.call(@db))
	end
      ensure
	@db = nil
      end

      return result
    end # def open

    # ----------------
    # -- ADD_STREAM --
    # ----------------

    def add_stream(stream_info)
      check_db_open
      raise Stream_Exists if @db.streams.has_key?(stream_info.unique_id) 


      @db.streams[stream_info.unique_id] = stream_info
      @db.stream_to_users[stream_info.unique_id] = Array.new

      return nil
    end

    # -------------------
    # -- REMOVE_STREAM --
    # -------------------

    def remove_stream(stream_id)
      check_db_open

      raise Stream_Unknown unless @db.streams.has_key?(stream_id)

      @db.stream_to_users[stream_id].each do |user_id| 
	remove_user(user_id, false) 
      end

      @db.stream_to_users.delete(stream_id)
      @db.streams.delete(stream_id)

      return nil
    end

    # -----------------
    # -- REMOVE_USER --
    # -----------------

    def remove_user(user_id, complete=true)
      check_db_open

      raise User_Unknown if not @db.users.has_key?(user_id)

      stream_id = @db.users[user_id].stream_id

      user = @db.users[user_id]

      user.upper_peers.dup.each {|upper| disconnect_peers(upper, user) }
      user.lower_peers.dup.each {|lower| disconnect_peers(user, lower) }

      @db.users.delete(user_id)
      if complete
	@db.stream_to_users[stream_id].delete(user_id)
      end

      return nil
    end

    # --------------
    # -- ADD_USER --
    # --------------

    def add_user(stream_id, user_info)
      check_db_open
      user_id   = user_info.unique_id

      raise Stream_Unknown  unless @db.streams.has_key?(stream_id)
      raise User_Exists     if     @db.users.has_key?(user_id)

      @db.users[user_id] = user_info

      @db.stream_to_users[stream_id] << user_id

      return nil
    end

    # -------------------
    # -- CONNECT_PEERS --
    # -------------------

    def connect_peers(upper, lower)
      check_db_open

      upper_id = get_id(upper)
      lower_id = get_id(lower)

      raise User_Unknown unless @db.users.has_key?(upper_id)
      raise User_Unknown unless @db.users.has_key?(lower_id)

      @db.users[upper_id].add_lower_peer(lower_id)
      @db.users[lower_id].add_upper_peer(upper_id)
    end

    # ----------------------
    # -- DISCONNECT_PEERS --
    # ----------------------

    def disconnect_peers(upper, lower)
      check_db_open


      upper_id = get_id(upper)
      lower_id = get_id(lower)

      # puts "disconnect #{upper_id} -> #{lower_id}"

      raise User_Unknown unless @db.users.has_key?(upper_id)
      raise User_Unknown unless @db.users.has_key?(lower_id)

      @db.users[upper_id].remove_lower_peer(lower_id)
      @db.users[lower_id].remove_upper_peer(upper_id)
    end

    # ----------------
    # -- GET_STREAM --
    # ----------------

    def get_stream(stream_id) 
      check_db_open
      return @db.streams[stream_id]
    end

    # ----------------------
    # -- GET_STREAM_USERS --
    # ----------------------

    def get_stream_users(stream_id) 
      check_db_open
      return @db.stream_to_users[stream_id].map {|user_id| @db.users[user_id].dup}
    end

    # ------------------
    # -- SET_VARIABLE --
    # ------------------

    def set_variable(name, value)
      check_db_open
      @db.variables[name] = value
    end

    # ------------------
    # -- GET_VARIABLE --
    # ------------------

    def get_variable(name)
      check_db_open
      return @db.variables[name]
    end

    private

    def check_db_open
      raise Unopened_DB if @db.nil?
    end

    def get_id(stream_info)
      return stream_info.is_a?(String) ? stream_info : stream_info.unique_id
    end
  end #class Marshalled_File
end
