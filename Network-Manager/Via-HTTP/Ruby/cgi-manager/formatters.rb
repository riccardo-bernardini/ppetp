require 'cgi-manager/utilities'

module Formatters
  @@name_to_class = Hash.new

  def Formatters::register(name, cl)
    @@name_to_class[name] = cl
  end

  def Formatters::get_formatter(type)
    Utilities::get(type, @@name_to_class)
  end

end

require 'cgi-manager/formatters-xml.rb'
