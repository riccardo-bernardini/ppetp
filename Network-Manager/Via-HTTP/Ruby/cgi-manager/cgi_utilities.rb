module CGI
  CGI::OK          = [200, "OK"]

  CGI::Bad_Request = [400, "Bad Request"]
  CGI::Forbidden   = [403, "Forbidden"]

  CGI::Internal_Error      = [500, "Internal Error"]
  CGI::Service_Unavailable = [503, "Service Unavailable"]

  class Reply
    #
    # This class represents an HTTP response.
    #
    def initialize(err_code, body=nil, headers=nil)
      #
      # The constructor can be called in several different ways
      #
      #   Reply.new(CGI::OK)               
      #      Empty body, default message
      #
      #   Reply.new([CGI::OK, "Cool!"])
      #      Empty body, message != default
      #
      #   Reply.new(CGI::OK, "<html>boh?!?</html>")
      #      Body, default message, default content-type
      #
      #   Reply.new(CGI::OK, "<html>boh?!?</html>", "text/html")
      #      Body, default message, content-type specified
      #
      #   Reply.new(CGI::OK, "<html>boh?!?</html>", { "h1" => "v1" })
      #      Body, default message, header h1 specified
      #
      #
      raise "bum" unless err_code.is_a?(Array)

      @err_code, @message = err_code

      if (@err_code.is_a?(Array))
	#
	# If I am here, err_code is something like
	#
	#    [CGI::OK, "Cool!"] = [[200, "OK"], "Cool!"]
	#
	# so that @err_code = [200, "OK"].  I must get only the
	# first entry of @err_code
	#
	@err_code = @err_code[0]
      end

      @body     = body
      #
      # @headers is an Hash that maps header names into
      # header values.  How it is constructed it depends on
      # the way the constructor was called
      #
      @headers  = case headers
		  when NilClass
		    Hash.new 
		  when String
		    { 'Content-type' => headers }
		  when Hash
		    headers
		  else
		    raise "bum"
		  end
    end

    def add_headers(new_headers)
      new_headers.each do |key, val|
	@headers[key] = val
      end
    end

    attr_reader   :err_code, :message, :headers
    attr_accessor :body
  end



  #
  # Few utility functions taken shamelessly from cgi.rb...
  #

  # URL-encode a string.
  #   url_encoded_string = CGI::escape("'Stop!' said Fred")
  #      # => "%27Stop%21%27+said+Fred"
  def CGI::escape(string)
    string.gsub(/([^ a-zA-Z0-9_.-]+)/n) do
      '%' + $1.unpack('H2' * $1.size).join('%').upcase
    end.tr(' ', '+')
  end


  # URL-decode a string.
  #   string = CGI::unescape("%27Stop%21%27+said+Fred")
  #      # => "'Stop!' said Fred"
  def CGI::unescape(string)
    string.tr('+', ' ').gsub(/((?:%[0-9a-fA-F]{2})+)/n) do
      [$1.delete('%')].pack('H*')
    end
  end

  def CGI::parse(query)
    params = Hash.new

    query.split(/[&;]/n).each do |pairs|
      key, value = pairs.split('=',2).collect{|v| CGI::unescape(v) }
      key=key.downcase
      if params.has_key?(key)
	if params[key].is_a?(Array)
	  params[key].push(value)
	else
	  params[key] = [params[key], value]
	end
      else
	params[key] = value
      end
    end

    return params
  end

  def CGI::write(reply, destination=$stdout)
    #
    # "Print" the HTTP response represented by reply.  It can be
    # called in several ways
    #
    #    CGI::write(reply)
    #       Print on standard output
    #
    #    CGI::write(reply, $stderr)
    #       Print on standard error
    #
    #    buffer=""
    #    CGI::write(reply, buffer)
    #       Append the result to buffer
    #
    # More generally, the second parameter can be anything that responds
    # to <<
    #
    headers = CGI::normalize_headers(reply.headers)
    body    = reply.body.to_s

    headers['content-length'] = body.length

    destination << "HTTP/1.1 #{reply.err_code} #{reply.message}\r\n"
    destination <<  CGI::make_header_block(headers)
    destination <<  "\r\n"
    destination <<  body
  end

  private

  def CGI::normalize_headers(headers)
    #
    # Force all the header names to downcase and add, if
    # necessary, default values for some headers
    #
    result = Hash.new
    headers.each do |name, value|
      result[name.downcase] = value.dup
    end

    defaults = { 
      "server"     => (ENV['SERVER_SOFTWARE'] or ""),
      "connection" => "close",
    }
    
    defaults.each do |name, value|
      next if result.has_key?(name)
      result[name] = value
    end

    return result
  end

  def CGI::camelize(s)
    return s.split('-').map {|x| x.capitalize}.join('-')
  end

  def CGI::make_header_block(headers)
    result = ''
    header_order = [
      "server",           "connection", 
      "content-type",     "content-length", 
      "content-language", "expires"
    ]

    header_order.each do |name|
      next if not headers.has_key?(name) 
      result += "#{CGI::camelize(name)}: #{headers[name]}\r\n"
      headers.delete(name)
    end
      
    headers.each do |name, value|
      result += "#{CGI::camelize(name)}: #{value}\r\n"
    end

    return result
  end
end
