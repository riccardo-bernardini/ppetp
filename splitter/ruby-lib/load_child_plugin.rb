module Utilities
  # ======================
  # == REQUIRE_MATCHING ==
  # ======================
  
  #
  # Search for files matching PATTERN (in "Unix glob format", 
  # e.g., "*.c") and require them.
  #
  def Utilities.require_matching(pattern)
    Dir[pattern].each {|filename| require filename }
  end


  # =========================
  # == LOAD_CHILD_PACKAGES ==
  # =========================
  
  #
  # Scan the directory <dir> (by default, the directory of 
  # the caller) for filenames whose name matches 
  # "<caller filename>-*.rb" and load them.  For example, 
  # if this function is called from file /tmp/test-ugly_hack.rb
  # it will load files matching /tmp/test-ugly_hack-*.rb, e.g., 
  #
  #    /tmp/test-ugly_hack-pippo.rb 
  #    /tmp/test-ugly_hack-pluto.rb
  #    /tmp/test-ugly_hack-paperino.rb
  #
  def Utilities.load_child_plugins(dir=nil, caller_filename=nil)
    if caller_filename.nil?
      #
      # Get the full filename of who called you
      #
      caller_filename, *junk = caller[0].split(':')
      caller_filename = File.expand_path(caller_filename)
    end

    if dir.is_a? Array
      # If dir is an array, process each entry recursively
      dir.each {|d| Utilities.load_child_plugins(d, basename) }
      return
    end

    basename = File.basename(caller_filename, '.rb')
    dirname  = File.dirname(caller_filename)

    #
    # By default search the child packages in the same directory
    # of the caller, otherwise use the parameter dir
    #
    if dir.nil?
      # No dir parameter: search in the caller directory
      pattern_head = File.join(dirname, basename)
    elsif dir[0..0] != '/'
      # Relative dir parameter: relative to the caller directory
      pattern_head = File.join(dirname, dir, basename)
    else
      # Absolute dir parameter
      pattern_head = File.join(dir, basename)
    end

    Utilities.require_matching("#{pattern_head}-*.rb")
  end
end
