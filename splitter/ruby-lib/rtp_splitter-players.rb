require 'load_child_plugin'

class Player_Root_Class
  @@player_classes = Array.new

  def initialize(name, port)
    @name = name
  end

  def die
    raise "Unimplemented"
  end

  def self.inherited(subclass)
    @@player_classes << subclass
  end


  def Player_Root_Class.handler_for(requested)
    result = nil
    best_confidence = -1

    @@player_classes.each do |handler_class|
      handler_class.handled_players.each do |entry|
	name, confidence = entry
	if name == requested and confidence >= best_confidence
	  result = handler_class
	  best_confidence = confidence
	end
      end
    end

    raise "Could not find controller for '#{name}'" if result.nil?

    return result
  end
end

Utilities.load_child_plugins
