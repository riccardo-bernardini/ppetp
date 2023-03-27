require 'cgi-manager/utilities'

module Allocators
  @@name_to_class = Hash.new

  def Allocators::register(name, cl)
    @@name_to_class[name] = cl
  end

  def Allocators::get_allocator(type)
    Utilities::get(type, @@name_to_class)
  end

  class Error < Exception
  end

  class No_Peers < Error 
  end

  def Allocators::ok?(x)
    return x.is_a?(Array)
  end

  def Allocators::peers(x)
    return x
  end
end

require 'cgi-manager/allocators-simple.rb'
