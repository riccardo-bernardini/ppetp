#
# This module implements a "toy P2P manager" to be used for testing 
# purposes.  The information about the P2P nodes are kept in a 
# text file with the following format
#
# <session 1 name>
#   <ip user 1>:<port>:<available streams>     <<-- Note: begins with ' '
#   <ip user 2>:<port>:<available streams>     
#   <ip user 3>:<port>:<available streams>     
# <session 2 name>
#   <ip user 1>:<port>:<available streams>     
#   <ip user 2>:<port>:<available streams>     
#
# That is, the file contains the informations relative to several
# "sessions".  The section relative to a sessions begin with a line
# with the session name (starting in column 1); the informations relative
# to the nodes belonging to the session are written in subsequent lines.
# Each node line begins with a space and it contains the IP addres,
# the port and the number of still available output streams separated
# by ':'.
#
# The two main methods provided are
#
#   create_stream(db_file, stream_name, roots=nil)
#
#   join_user(db_file, stream, user, requested_streams)
#
# A root is a node that transmits only and requires no input stream (so,
# it can be interpreted as a user with requested_streams==0).  The user
# parameter above is an object of class P2P_Manager_Tree::User. The
# initialize method of P2P_Manager_Tree::User accepts the three
# values (ip, port, N. of streams) both as three separated values and
# as a string of form <ip>:<port>:<N. of streams>.
#
module P2P_Manager_Tree

  class P2P_Manager_Tree::User
    def initialize(arg1, arg2=nil, arg3=nil)
      if (arg2 and arg3) 
	ip_address  = arg1
	port        = arg2
	max_streams = arg3
      else
	ip_address, port, max_streams = arg1.split(':')
      end

      @ip_address  = ip_address
      @port        = port
      @max_streams = max_streams.to_i
    end

    def <=>(other)
      return self.max_streams <=> other.max_streams
    end

    def gimme_stream
      @max_streams = @max_streams - 1
    end

    def to_s
      return "#{@ip_address}:#{@port}:#{@max_streams}"
    end

    attr_reader :ip_address, :port, :max_streams
  end

  class P2P_Manager_Tree::Server_DB
    def initialize(arg1=nil)
      @name_to_db = Hash.new
      if (arg1)
	current_stream_db = nil
	all_lines = arg1.split($/)
	all_lines.each do |line| 
	  continue if (line.strip == '')

	  if (line[0..0] == ' ')
	    raise "Bum" if (current_stream_db.nil?)
	    current_stream_db << User.new(line.strip)
	  else
	    current_stream_db = Stream_DB.new(line)
	    self[line.strip]=current_stream_db
	  end
	end
      end
    end

    def []=(stream_name, stream_db)
      raise "Wrong" unless stream_db.is_a? P2P_Manager_Tree::Stream_DB
      # raise "Duplicate" if @name_to_db.include?(stream_name)
      @name_to_db[stream_name] = stream_db
    end

    def [](stream_name)
      return @name_to_db[stream_name]
    end

    def include?(stream_name)
      return @name_to_db.include?(stream_name)
    end

    def to_s
      result = []

      @name_to_db.each do |stream_name, stream_db|
	result << stream_name 
	result += stream_db.to_s.split("\n").map {|line| " " + line }
      end

      return result.join("\n")
    end
  end

  class P2P_Manager_Tree::Stream_DB
    def initialize(stream_name)
      raise "Wrong" unless stream_name.is_a? String

      @user_list = Array.new
      @stream_name = stream_name
    end

    attr_reader :stream_name

    def <<(new_user)
      raise "Wrong" unless new_user.is_a?(User)
      @user_list << new_user
      @user_list.sort!
    end

    def next_free_stream(n_streams)
      return '' if n_streams==0
      # p @user_list, n_streams
      raise "Out of streams" unless @user_list[-n_streams].max_streams > 0

      result = (-n_streams .. -1).map do |idx|
	ip_addr = @user_list[idx].ip_address
	port    = @user_list[idx].port
	@user_list[idx].gimme_stream
	[ip_addr, port]
      end

      @user_list.sort!

      return result
    end

    def to_s
      result = @user_list.map { |user| user.to_s }
      return result.join("\n");
    end
  end

  def P2P_Manager_Tree::load_db(db_file_name)
    File.open(db_file_name) do |input|
      return P2P_Manager_Tree::Server_DB.new(input.read)
    end
  end

  def P2P_Manager_Tree::save_db(server_db, db_file_name)
    File.open(db_file_name, 'w') do |output|
      output.puts(server_db.to_s)
    end
  end

  def P2P_Manager_Tree::with_file_locked(filename) 
    File.new(filename).flock(File::LOCK_EX)
    yield
    File.new(filename).flock(File::LOCK_UN)
  end


  def P2P_Manager_Tree::create_stream(db_file, stream_name, force, roots=nil)
    with_file_locked(db_file) do
      server_db = P2P_Manager_Tree::load_db(db_file)

      if (server_db[stream_name] and not force)
	return [false, "Stream exists"] 
      end

      stream_db = Stream_DB.new(stream_name)
      server_db[stream_name] = stream_db

      if (roots.is_a? User)
	stream_db << roots
      elsif  (roots.is_a? Array)
	roots.each { |x| stream_db << x }
      elsif (roots.nil?)
	# Ignore
      else
	raise "Boh"
      end

      P2P_Manager_Tree::save_db(server_db, db_file)
    end
    return true
  end

  def  P2P_Manager_Tree::join_user(db_file, stream, user, requested_streams)

    result = nil
    with_file_locked(db_file) do
      server_db = P2P_Manager_Tree::load_db(db_file)
      stream_db = server_db[stream]

      return [false, "Unknown stream"] if stream_db.nil?

      result = stream_db.next_free_stream(requested_streams);

      stream_db << user

      P2P_Manager_Tree::save_db(server_db, db_file)
    end

    return [true, result]
  end

  def P2P_Manager_Tree::init_db(db_file)
    with_file_locked(db_file) do
      File.open(db_file, 'w') do |st| 
	st.truncate(0) 
      end
    end
  end
end


if false
  def parse(string)
    result = Hash.new
    string.split('&').each do |pair|
      name, value = pair.split('=')
      result[name] = value
    end

    return result
  end

  include P2P_Manager_Tree

  db_file_name = "/tmp/p2p-manager-db.txt"

  if ARGV.size < 2
    puts "Too few arguments"
    exit 1
  end

  command = ARGV.shift
  param = parse(ARGV.shift)

  case command.upcase
  when "CREATE"
    if (param['root'])
      P2P_Manager_Tree::create_stream(param["stream"], 
				      User.new(param["root"]), 
				      db_file_name)
    else
      P2P_Manager_Tree::create_stream(param["stream"], 
				      nil, 
				      db_file_name)
    end
  when "JOIN_USER"
    addresses = P2P_Manager_Tree::join_user(param["stream"], 
					    User.new(param["user"]), 
					    db_file_name, 3)
    
    if (not addresses.empty?)
      puts addresses.map {|x| x.join(' ')}.join("\n")
    end
  when "JOIN_ROOT"
    P2P_Manager_Tree::join_user(param["stream"], 
				User.new(param["root"]), 
				db_file_name, 
				0)
  else
    puts "Unrecognized"
  end
end
