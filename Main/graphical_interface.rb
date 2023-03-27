require 'tk'

def quote_url(x)
  x = x.gsub('%', sprintf('%%%02X', ?%))

  while x =~ /[^-0-9a-zA-Z$_.+!*:(),%\/']/
    x = x.gsub($MATCH, sprintf('%%%02X', $MATCH[0]))
  end

  return x
end

class Graphical_Interface
  def initialize(command_queue)
#    spec_array = 
#      [ :root, { title "MEDUSA" },
#      [ :vbox,
#	[ :entry, :address, :address_text ],
#	[ :canvas ],
#	[ :flushleft,
#	  [ :button, :open_button ],
#	  [ :button, :quit_button ]]]]

    @command_queue = command_queue
    
    @root = TkRoot.new { title "MEDUSA" }

    @address_text = TkVariable.new
    @address = TkEntry.new(@root) do
      pack("fill" => "x")
    end
    @address.textvariable(@address_text)
    @address.bind("Return") { do_open }
    @address.value= "http://127.0.0.1:55505/ciao"

    @canvas = TkCanvas.new(@root) do
      height 0
      width  0
    end.pack("padx" => 15, "after" => @address)


    frame = TkFrame.new(@root) do
      height 20
    end.pack("fill" => "x", "after" => @canvas)

    @open_button = TkButton.new(frame) do
      text "Play"
      pack("side" => :left)
    end

    @open_button.command {do_open}

    @quit_button = TkButton.new(frame) do
      text "Quit"
      pack("side" => :left)
    end

    @quit_button.command {do_quit}
  end

  def viewport
    return TkWinfo.id(@canvas)
  end

  def set_viewport_size(w, h)
    @canvas.configure('width' => w, 'height' => h) 
  end

  def do_open
    @command_queue << Request.new("GUI.OPEN #{quote_url(@address_text.to_s)}")
  end

  def do_quit 
    @command_queue << Request.new("GUI.QUIT")
  end

  def mainloop
    Tk.mainloop
  end
end
