require 'cgi'
require 'zlib'

module Command_Formatter
  class Timestamp
    @current=-1
    def Timestamp.current
      return "%06X" % @current
    end

    def Timestamp.next
      @current += 1
      return Timestamp.current
    end
  end

  def Command_Formatter.add_crc(s)
    s + " " + ("%04X" % Zlib.crc32(s))
  end


  def Command_Formatter.format_command(*args)
    # if (args.size == 1 && args[0].is_a?(Array))
    #   args=args[0]
    # end
    fields = args.flatten
    complete = (fields[-1] == :complete)

    if complete
      fields.pop
      command = fields.shift
      timestamp = Timestamp.next 
      fields = [command, timestamp] + fields
    end

    result = fields.map {|x| x.gsub(' ', '%20')}.join(" ")
    result = add_crc(result) if complete
    
    return result
  end
end

#
# After adding the following function I discovered that there is a 
# crc32 method in Zlib (require 'zlib').  Nevertheless, I am leaving 
# the code here for any evenience...
#
### def crc32(c)
###   #
###   # Ruby implementation of CRC32.  Found on Ruby newsgroup.
###   # See http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-talk/48643
###   #
###   # How it comes that Ruby has not a built-in implementation of CRC32?!?
###   #
###   r = 0xFFFFFFFF
###   c.each_byte do |b|
###     r ^= b
###     8.times do
###       r = (r>>1) ^ (0xEDB88320 * (r & 1))
###     end
###   end
###   r ^ 0xFFFFFFFF
### end
