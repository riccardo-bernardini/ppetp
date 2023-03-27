require 'cgi-manager/utilities'

module Authenticators
  @@name_to_class = Hash.new

  def Authenticators::register(name, cl)
    @@name_to_class[name] = cl
  end

  def Authenticators::get_authenticator(type)
    Utilities::get(type, @@name_to_class)
  end
end

require 'cgi-manager/authenticators-null'
