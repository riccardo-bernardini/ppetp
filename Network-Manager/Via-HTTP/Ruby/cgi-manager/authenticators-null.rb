module Authenticators
  class Null_Authenticator
    Authenticators::register('null', Null_Authenticator)

    def check_query(query, param)
      return true
    end
  end
end

require 'cgi-manager/authenticators-null'
