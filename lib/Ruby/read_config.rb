#!/usr/bin/env ruby

class Config_Data
  def Config_Data.read (*args)
    if (args.size == 1 and args[0].is_a?(Hash))
      args = args[0]
      filenames = args[:files]
      args.delete(:files)
      options = args
    else
      options = Hash.new
      filenames = args
    end
    data = Config_Data.new(options)
    filenames.each {|name| data.read(name)}
    return data
  end

  def initialize(opts)
    opts_names = { 
      :types          => { :default => Hash.new },
      :on_undeclared  => { :default => :accept },
      :untrimmed      => { :default => Array.new }}
      
    opts_names.each do |name, data|
      if (opts.has_key?(name))
	value = opts[name];
      else
	value = data[:default]
      end

      eval("@#{name}=value")
    end

    @data = Hash.new
  end

  def read(filename)
    File.open(filename) do |stream|
      stream.each do |line|
	next if     line =~ /^ *(#.*)?$/
	next unless line =~ /^([^ =]+) *=(.*)$/
	name=$1
	value=$2

	type=@types[name] || @types[name.to_sym]

	if (type)
	  value = eval("#{type.to_s}(value)")
	else
	  case @on_undeclared
	  when :accept
	    # Nothing to do
	  when :die
	    raise "pippo"
	  when :ignore
	    next
	  else
	    raise "pluto"
	  end
	end
	  
	if (value.is_a?(String) and not @untrimmed.include?(name))
	  value=value.strip
	end

	@data[name]=value 
      end
    end
  end

  def [](name)
    @data[name.to_s]
  end
end

if ($0 == __FILE__)
  tmp='/tmp/prova-rdconf.txt'
  File.open(tmp, 'w') do |stream|
    DATA.each { |x| stream.print x }
  end

  u = Config_Data.read(:types => { :pippo => Integer },
		       :files => tmp)
  p u[:pippo]
  p u['pippo']
end

__END__
pippo=42
zorro=~/jl
