module Utilities
  def Utilities::get(type, name_to_class)
    idx = type.index(':')
    if idx.nil?
      param=nil
    else
      param = type[idx+1..-1]
      type  = type[0..idx-1]
    end
      
    return nil unless name_to_class.has_key?(type)

    if param.nil?
      return name_to_class[type].new
    else
      return name_to_class[type].new(param)
    end
  end
end
