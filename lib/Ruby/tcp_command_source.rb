require 'format_command'

Request = Struct.new(:command, :socket)
Reply   = Struct.new(:request, :msg)

class TCP_Command_Source
  #
  # When you create a TCP_Command_Source a new thread is started.
  # Such a thread 
  #
  #    1) open the TCP port given to new and begin listening.  
  #
  #    2) When a new connection arrives, it reads the socket
  #       until the client does a "write shutdown".
  #
  #    3) Insert a new request with the string just read in the 
  #       queue given to the :new method at creation time.
  #
  # The request will be read by some other thread which will 
  # process it.  The processing thread can send a reply to the 
  # remote client via the TCP_Command_Source#reply_to method
  #
  def initialize(port, command_queue)
    @command_queue = command_queue
    @reply_queue = Queue.new

    @com_thr = input_thread(port, @command_queue)
    @reply_thr = output_thread(@reply_queue)
  end

  def reply_to(request, msg)
    if (msg.is_a? Array)
      msg=Command_Formatter.format_command(msg)
    end

    @reply_queue << Reply.new(request, msg)
  end

  private

  def input_thread(port, command_queue)
    Thread.new(port, command_queue) do |port, com_queue|
      server_socket = TCPServer.new(port)
      while true
	sock = server_socket.accept
	command = sock.gets
	# puts "Ricevuto [#{command}]"

	sock.shutdown(0)
	com_queue << Request.new(command, sock)

      end
    end
  end

  def output_thread(queue)
    Thread.new(queue) do |reply_queue|
      while true
	reply = reply_queue.pop
	sock = reply.request.socket
	
	begin
	  sock << reply.msg unless (reply.msg.nil?)
	  sock.shutdown(1)
	rescue SystemCallError
	  # Nothing to do
	  #
	  # If I am here, the peer closed the connection
	  # before I could answer.  No big deal...
	  #
	ensure
	  sock.close
	end
      end
    end
  end
end
