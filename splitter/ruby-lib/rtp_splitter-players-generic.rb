class Generic_Player < Player_Root_Class
  @@player_table = {
    'vlc' => {
      'builder' => proc {|host, port| ["vlc", "rtp://@#{host}:#{port}"] },
      'signal'  => "SIGKILL"
    },
   'g-audio' => {
      'builder' => proc {|host, port| ["./players/g-audio-player",  host.to_s, port.to_s] },
      'signal'  => "SIGQUIT"
    },
   'g-video' => {
      'builder' => proc {|host, port| ["./players/g-video-player", host, port] },
      'signal'  => "SIGQUIT"
    }
  }

  def Generic_Player.handled_players
    p 'zimino!'
    return @@player_table.keys.map {|name| [name, 0]}
  end

  def initialize(name, port, host='127.0.0.1')
    p "gestisco #{name}!"
    @name = name.downcase
    @player_info = @@player_table[@name]

    raise "Player '#{name}' unimplemented" if @player_info.nil?

    @pid = nil

    start(host, port)
  end

  def pid
	return @pid
  end


  def die
    return [false, "Not started"] unless running?
    count = 0

    begin
      Process.kill(@player_info['signal'], @pid)
      timeout(5) do
	Process.waitpid(@pid)
      end
    rescue Errno::ESRCH => msg
      # Ignore exception raised if @pid does not exist
      # It means that the process does not exist anymore
    rescue Timeout::Error
      # Timeout on waitpid: try at most ten times,
      # then give up
      return [false, "Cannot kill"] if count == 10
      count += 1
      retry
    end

    p "#{@pid} killed"
    @pid = nil
    return [true, 'ok']
  end

  def running?
    return ! @pid.nil?
  end

  def start(host, port)
    return [false, "Already started"] if running?
    pid = fork
    if pid.nil?
      p "Running '#{@player_info['builder'].call(host, port)}'"
      params = @player_info['builder'].call(host, port)
	   exec(*params)
      # never get here
    else
      @pid = pid
      @started = true
      return [true, 'ok']
    end
  end
end
