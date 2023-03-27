module Config
  @@config = Hash.new

  def Config::get(name, default=nil, type=nil)
    val = (@@config[name] || default)
    if (type.nil? and default.nil?)
      # No conversion required: return the value as it is
      return val
    end
    
    if type.nil?
      # Note: Here type was not specified, but default was since 
      # here (not default.nil?) is true because of the 'if' above.
      # We choose "type" on the basis of the default value
      case default
      when String
	type = 'string'
      when Fixnum
	type = 'int'
      when Float
	type = 'float'
      when TrueClass, FalseClass
	type = 'bool'
      end
    end

    #
    # Convert, if requested, val to type
    #
    case type.to_s.downcase
    when nil
      return val
    when 'string', 's'
      return val.to_s
    when 'int', 'i'
      return val.to_i
    when 'float', 'f'
      return val.to_f
    when 'bool', 'b'
      return (val ? true : false)
    end
  end

  def Config::set(name, value)
    @@config[name] = value
  end

  def Config::append(name, value)
    if (@@config[name].nil?)
      @@config[name] = []
    elsif (not (@@config[name].is_a? Array))
      @@config[name] = [@@config[name]]
    end

    @@config[name] << value
  end
end
