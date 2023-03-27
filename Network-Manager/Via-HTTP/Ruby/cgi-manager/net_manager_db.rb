require 'yaml'

module NetManager_DB
  @@name_to_class = Hash.new

  def NetManager_DB::register(name, cl)
    @@name_to_class[name] = cl
  end

  def NetManager_DB::get_db_handler(type)
    Utilities::get(type, @@name_to_class)
  end

  class DB_Error < Exception
  end

  class Stream_Exists < DB_Error
  end

  class Stream_Unknown < DB_Error
  end

  class User_Unknown < DB_Error
  end

  class User_Exists < DB_Error
  end

  class Unopened_DB < DB_Error
  end
end

require 'cgi-manager/net_manager_db-file'
