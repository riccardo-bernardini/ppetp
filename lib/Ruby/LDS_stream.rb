class LDS_Stream
  include Enumerable

  def initialize(str)
    @buffer = str
    @regexp = /^\D*(\d+)(\D)/
    @err = :ok
  end

  def gets
    return nil if @buffer.empty?

    data = @regexp.match(@buffer)

    if (data.nil? || data[2] != ':')
      @buffer = ''
      @err = (data.nil?) ? :eof : :err
      return nil
    end

    len = data[1].to_i
    tail = data.post_match

    result  = tail[0..len-1]
    @buffer = tail[len..-1]

    return result
  end

  def each
    while (s=self.gets)
      yield(s)
    end
  end

  def readlines
    u = self.map
  end

  def LDS_Stream.readlines(str)
    LDS_Stream.new(str).readlines
  end
end

def LDS_Encode(s)
  return "#{s.length}:#{s}"
end
