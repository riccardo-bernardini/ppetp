#!/usr/bin/env ruby

require 'socket'
require 'cgi'
require 'thread'
require 'LDS_stream'

module Internal_State
  #
  # A Server is a kind of Hash table (whose keys and values are both
  # of string type) which can be interrogated by several processes,
  # even on different computers.
  #
  # = Main method: +consider+
  # The basic method of a Server is +consider+ which expects two
  # parameters: a +request_string+ and an object +reply_to+ which
  # must respond to methods +<<+ and +close+.  When the answer to the
  # query is ready, to +reply_to+ a string with the error code and any
  # text resulting from the query.  
  #
  # = Other methods
  # A Server responds also to methods +[]+ (to interrogate the Hash
  # table) and +[]=+ to write the Hash table.  Method +[]+ can accept
  # both a string and an array of strings (to do several queries at
  # once in an atomic fashion)
  #
  # Moreover, Server responds to +listen_to_port+ which requires a
  # TCP port number to be used for reception.  Several istances of
  # +listen_to_port+ can be called.
  #
  # = Query format
  # Currently there are two possible queries
  #
  #   * +GET+ _var1_ [_var2_ ...] [_timeout_]
  #   * +SET+ _var_ _value_
  #
  # The meaning of queries +SET+ and +GET+ should be obvious.  It is
  # necessary only to say that if a variable is not currently present
  # the Server waits for the variable to appear.
  #
  # = Result format
  #    The string passed to +<<+ always begin with a 3-digit code,
  #    followed by a space, a uman-readble string and a newline.  Any
  #    text resulting by the query is coded in a length-delimited
  #    format (a string with n characters is encoded as the value of
  #    n, the ':' character and the string)
  #
  #
  class Server
    class ::Queue 
      def close
      end
      def shutdown(x)
      end

      def closed?
	false
      end
    end

    class Request
      @@current_id = 0

      def initialize(request, reply_to)
	@reply_to = reply_to

	@command, @parameters = Internal_State::shift_word(request)
	@command.upcase!

	@id = @@current_id

	# puts "request # #{@id} -> #{request}[#{@command}][#{@parameters}]"
	@@current_id = 	@@current_id +1
	@expired = false
      end

      attr_reader :reply_to, :command, :id
      attr_accessor :timeout, :expired, :parameters

      def to_s
	"#{@command} #{@parameters.join(' ')} (exp=#{@expired}, id=#{@id})"
      end
    end # class Request


    def initialize
      @var_table = Hash.new
      @sessions = Array.new
      @ready_queue = Queue.new
      @waiting_for = Hash.new
      @consider_mutex = Mutex.new

      Thread.new do
	while true
	  process(@ready_queue.pop)
	end
      end
    end

    def consider(request_string, reply_to)
      @ready_queue << Request.new(request_string, reply_to)
    end

    def [](var_name)
      array_input = var_name.is_a?(Array)

      if (array_input)
	var_name = var_name.join(' ');
      end

      queue = Queue.new
      consider("GET #{var_name}", queue);
      query_result = queue.pop;

      idx = query_result.index("\n");
      return nil if idx.nil?;

      code = query_result[0..2];
      return nil unless code == "000"

      result = LDS_Stream.readlines(query_result[idx+1..-1])
      return nil if result.empty?

      if (not array_input)
	result = result[0]
      end

      return result
    end

    def []=(var_name, value)
      queue = Queue.new
      consider("SET #{var_name} #{LDS_Encode(value.to_s)}", queue);
      queue.pop;      
    end

    def listen_to_port(port, allowed=nil)
      if (allowed.is_a? String)
	allowed=[allowed]
      end

      thr = Thread.new(self, allowed) {|server, allowed|
	tcp_entry = TCPServer.new(port)
	while true
	  session = tcp_entry.accept

	  if (not allowed.nil?)
	    family, port, remote_name, remote_ip=session.addr
	    unless (allowed.include?(remote_ip) or
		    allowed.include?(remote_name))
	      session.shutdown(1)
	      continue
	    end
	  end

	  command = session.gets
	  if (command)
	    server.consider(command, session)
	  else
	    # p command
	  end
	end
      }
 
      return thr
    end

    def expired(request)
      request.expired = true
    end

    private 

    def add_to_waiting(request, var_name)
      # p "Adding #{request.to_s} to wait list for #{var_name}"
      unless @waiting_for.has_key?(var_name)
	@waiting_for[var_name] = Array.new
      end

      @waiting_for[var_name] << request	
    end

    # def remove_from_waiting(request, var_name)
    #   ok = @waiting_for[var_name].find {|x| x.id == request.id}
    #   return nil unless ok
    #   
    #   @waiting_for[var_name].delete_if {|x| x.id == request.id}
    # end

    def clear_waiting(var_name)
      # p @waiting_for
      return [] unless @waiting_for.has_key?(var_name)
      
      result = @waiting_for[var_name];
      @waiting_for.delete(var_name);
      result.each do |query|
	@timeout_server.cancel(query.timeout) unless query.timeout.nil?
	query.expired = false
      end

      return result
    end 

    def wait_for(request, var_name, timeout)
      timeout_id = nil;

      if (not timeout.nil?)
	if (timeout==0)
	  reply_unknown(request, var_name);
	  return
	else
	  timeout_id = @timeout_server.schedule(timeout, request)
	end
      end

      request.timeout = timeout_id
      add_to_waiting(request, var_name);
    end

    def reply(request, message, others)
      # p "REPLY-TO: #{request.reply_to} for #{request}"
      return if request.reply_to.nil?
      # p "REPLY-TOx: #{request.reply_to} for #{request}"
      if (not others.nil?)
	tmp = others.map {|str| sprintf('%d:%s', str.size, str)};
	message = message + "\n" + tmp.join("\n");
      end

      # p "REPLY-TOy: '#{message}' for #{request} closed=#{request.reply_to.closed?}"
      request.reply_to << message + "\n";
      c=request.reply_to.shutdown(1);
    end

    def reply_ok(request, result=nil)
      reply(request, '000 OK', result);
    end

    def reply_malformed(request, result=nil)
      reply(request, '001 MALFORMED', result);
    end

    def reply_unknown(request, var_name)
      reply(request, "002 UNKNOWN #{var_name}", nil);
    end
    
    def do_get(request)
      unless (request.parameters.is_a?(Array))
	request.parameters = request.parameters.split 
      end

      result = Array.new
      timeout = nil
      if (request.parameters[-1] =~ /^\d+(\.\d*)?$/)
	timeout = request.parameters.pop.to_i
      end

      request.parameters.each do |var_name|
	if @var_table.has_key?(var_name) 
	  result << @var_table[var_name];
	else
	  wait_for(request, var_name, timeout);
	  return
	end
      end
      
      # p "FATTO #{request.to_s}"

      reply_ok(request, result)
    end

    def do_set(request)
      var_name, value = Internal_State::shift_word(request.parameters)

      begin
	value = LDS_Stream.readlines(value)
      rescue Malformed_LDS
	reply_malformed(request);
	return
      end

      if (value.size != 1)
	reply_malformed(request);
	return
      end

      value = value[0]
      # puts "'#{value}' -> '#{var_name}'"

      @var_table[var_name] = value;
      # p @var_name
      clear_waiting(var_name).each {|x| @ready_queue << x}

      # p @ready_queue

      reply_ok(request);
    end

    def process(request)
      # p "Processing '#{request.to_s}'"
      return if request.expired 

      case request.command
      when 'GET'
	do_get(request);
      when 'SET'
	do_set(request);
      end
    end
  end

  class Connection
    def initialize(port, host=nil)
      host='127.0.0.1' if host.nil?
      @host = host
      @port = port
    end

    def get(*varnames)
      single_input=false
      if (varnames.size == 1) 
	if (varnames[0].is_a?(Array))
	  varnames = varnames[0];
	else
	  single_input=true;
	end
      end

      query="GET #{varnames.join(' ')}"
      # p "QUERY='#{query}'"
      connection = TCPSocket.new(@host, @port)
      connection.puts(query);
      connection.shutdown(1)
      result=connection.read
      # p "Result for '#{query}': #{result.inspect}"
      return Internal_State::parse_result(result, single_input)
    end

    def set(varname, value)
      connection = TCPSocket.new(@host, @port)
      connection.puts("SET #{varname} #{LDS_Encode(value.to_s)}");
      return 
    end

    def []=(varname, value)
      set(varname, value)
    end

    def [](varname)
      ok, code, data = self.get(varname)
      return ok ? data : nil
    end
  end

  def Internal_State::parse_result(result, single_input=false)
    code = result[0..2];
    ok = (code == "000")
    data = []

    if (ok)
      idx = result.index("\n");

      data = LDS_Stream.readlines(result[idx+1..-1]);

      data = data[0] if (data.size==1 and single_input)
    end

    return [ok, code, data]
  end

  def Internal_State::shift_word(string)
    string =~ /^ *([^ ]*)( +([^ ].*)?)?$/
    return [$1, $3 || '']
  end
end

if __FILE__ == $0 
  server = Internal_State::Server.new

  # thr = Thread.new(server) { |server|
  #   tcp_entry = TCPServer.new(54321)
  #   while true
  #     session = tcp_entry.accept
  #     server.consider(session.gets, session)
  #   end
  # }

  thr = server.listen_to_port(54321, '127.0.0.1') 
  puts server[['pippo', 'pluto']]
  server['zorro']='ciao a tutti'
  thr.join
end



