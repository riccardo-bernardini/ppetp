require 'thread'

class External_Module
  #
  # This class represents an internal module inside MEDUSA.  At creation
  # time the external program implementing the module is started and
  # its port read from the internal DB.
  #
  # Via methods << and puts the main program can send commands to the 
  # external module.
  #
  def initialize(config, module_name)
    @module_name = module_name.upcase;

    #
    # Get the port number of the central DB and the executable
    # associated with this module
    #
    server_port = config['state-port'];
    bin         = config["#{@module_name}-bin"];

    #
    # If the program name begin with '%' simply "fake" the 
    # module presence.  This can be useful for testin purposes.
    #
    if (bin[0] == ?%)
      @fake = true;
      return
    else
      @fake = false;
    end

    #
    # Replace an initial '~' with the base dir of MEDUSA
    #
    if (bin[0] == ?~)
      bin = '$MEDUSA_HOME' + bin[1..-1];
    end

    bin = bin.gsub('$MEDUSA_HOME', ENV['MEDUSA_HOME']);

    #
    # By default the port number is the last parameter
    #
    if (bin.index('%').nil?)
      bin += ' %d' 
    end

    #
    # Create the actual command line and process it in a separate process
    #
    command = sprintf(bin, server_port)
    # $stdout.puts "For #{module_name} use #{command}"

    #
    # Create the new process with 'popen' and not with 'fork'
    # because 'fork' is not implemented in Windows.
    #
    tmp = File.popen(command, 'w')
    @pid = tmp.pid

    #
    # Get the number of the port used by the new module to receive
    # commands.
    #
    @module_port = State_Server["#{@module_name}.INTERNAL_PORT"].to_i;

    @mutex = Mutex.new
  end

  def <<(x)
    self.puts(x)
  end

  def puts(x)
    return if @fake
    @mutex.synchronize do 
      socket = TCPSocket.open('127.0.0.1', @module_port)
      socket.send(x, 0)
      socket.shutdown(1)
    end
  end

  def quit
    return if @fake

    #
    # First try to kill the subprocess gently by 
    # sending it  a "QUIT" command
    #
    self.puts("QUIT")
    sleep(0.1)

    #
    # Since we cannot be sure that every implementation will
    # honor "QUIT", we use "maniere forti" by sending a kill
    # signal to the subprocess.  However, if the process honored 
    # the "QUIT" command trying to kill it will raise an
    # exception (that we will intercept with "rescue").
    #
    begin
      Process.kill("SIGKILL", @pid)
      Process.wait(@pid)
    rescue SystemCallError
      # Nothing to do
      # If I am here, the process exited because of the command "QUIT"
    end

    #
    # Setting @fake to true will prevent any future interaction
    # with the external module
    #
    @fake = true
  end
end
