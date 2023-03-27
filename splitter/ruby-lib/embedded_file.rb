class Embedded_File
  #
  # This is a nice class that allows to use the DATA constant as a
  # "virtual filesystem".  You just open a "virtual file" with
  #
  #                   f = Embedded_File.new(path)
  #
  # then you can use f with most of the IO based reading functions 
  # (i.e., getc, gets, each, ....)
  #

  #
  # @@files will hold an Hash table mapping filenames to content
  #
  @@files = nil

  def Embedded_File.use(source)
    @@files = Hash.new
    data = source.read
    return if data.empty?
    while not data.empty?
      #
      # Separate the header line from the rest of data
      #
      header, data = data.split($/, 2)
      raise IOError if data.nil?

      #
      # Get the "EOF marker" and the filename
      #
      marker, name = header.split(/ +/, 2)

      #
      # Separate content and rest of data
      #
      content, data = data.split(marker + $/, 2)
      raise IOError if data.nil?

      @@files[name]=content
    end
  end


  def Embedded_File.init
    Embedded_File.use(DATA) if @@files.nil?
  end

  def Embedded_File.dir
    Embedded_File.init
    return @@files.keys
  end

  def initialize(path)
    Embedded_File.init

    raise Errno::ENOENT.new(path) unless @@files.has_key?(path)

    @data = @@files[path]
    @path = path
    @next_char = 0
    @open = true
  end

  def getc
    raise IOError unless @open
    return nil if eof?

    result = @data[@next_char]
    @next_char += 1 unless result.nil?
    return result
  end

  def gets(separator=$/)
    raise IOError unless @open
    return nil if eof?

    @data[@next_char..-1].index($/)
    line_length = @data[@next_char..-1].index($/)

    eol = line_length.nil? ? (@data.size-1) :  (@next_char + line_length)

    result = @data[@next_char..eol]

    @next_char = eol+1

    return result
  end

  def closed?
    return !@open
  end

  attr_reader :path

  def eof?
    raise IOError unless @open
    return @next_char >= @data.size
  end

  alias :eof :eof?

  def read(length=nil)
    raise IOError unless @open
    length = @data.size - @next_char if length.nil?
    last_char = @next_char + length-1
    result = @data[@next_char..last_char]
    @next_char = last_char+1

    return result
  end

  def seek(where, from = IO::SEEK_SET)
    raise IOError unless @open
    where = case from
	    when IO::SEEK_CUR
	      where + @next_char
	    when IO::SEEK_END
	      where + @data.size - 1
	    when IO::SEEK_SET
	      where
	    else
	      raise Errno::EINVAL
	    end

    @next_char = [[where, 0].max, @data.size].min
  end

  def pos
    raise IOError unless @open
    @next_char
  end

  alias :tell  :pos 

  def close
    raise IOError unless @open
    @open = false
  end

  def each_line(sep=$/)
    while line=gets(sep)
      yield(line)
    end
  end

  alias :foreach :each_line 
  alias :each    :each_line 

  def each_byte
    while c=getc
      yield(c)
    end

    return nil
  end

  def readline(sep=$/)
    raise EOFError if eof?
    gets(sep)
  end

  def readchar
    raise EOFError if eof?
    getc
  end

  def readlines(sep=$/)
    result = []
    each_line(sep) {|x| result << x}
    return result
  end

  def rewind
    seek(0)
  end

  def pos=(where)
    seek(where)
  end

  def Embedded_File.open(path)
    file = Embedded_File.new(path)
    return file if not block_given? 

    result = yield(file)
    file.close
    return result
  end
end
