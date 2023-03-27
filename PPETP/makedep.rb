#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

class Bool_Matrix 
  def initialize (nrow, ncol=nil)
    ncol=nrow if (ncol.nil?)
    @nrow = nrow
    @ncol = ncol
    @data = (1..nrow).map {|x| Array.new(ncol, false)}
  end

  def [](r,c) 
    @data[r][c]
  end

  def []=(r,c,value) 
    @data[r][c]=value
  end

  def *(other) 
    raise "Wrong dim" unless @ncol == other.nrow

    result = Bool_Matrix.new(@nrow, other.ncol)
    
    (0...result.nrow).each do |row|
      (0...result.ncol).each do |col|
	value=false
	(0...@ncol).each do |internal|
	  if (@data[row][internal] and other.data[internal][col])
	    value=true
	    break
	  end
	end # internal

	result[row,col]=value

      end # col
    end # row

    return result
  end

  def +(other) 
    raise "Wrong dim" unless (@ncol == other.ncol and @nrow == other.nrow)
    result = Bool_Matrix.new(@ncol, @nrow)

    (0...result.nrow).each do |row|
      (0...result.ncol).each do |col|
	result.data[row][col] = (@data[row][col] || other.data[row][col])
      end
    end

    return result
  end

  def eye!
    (0...@nrow).each do |row|
      (0...@ncol).each do |col|
	@data[row][col] = (row==col)
      end
    end
  end

  def ==(other)
    raise "Wrong dim" unless (@ncol == other.ncol and @nrow == other.nrow)

    (0...@nrow).each do |row|
      (0...@ncol).each do |col|
	return false if @data[row][col] != other.data[row][col]
      end
    end

    return true
  end

  attr_reader :nrow, :ncol, :data
end

class Dati
  def initialize(base, dip)
    @base = base
    @dip  = dip
  end 

  attr_reader :base, :dip

  def <=>(other)
    result = (@dip.size <=> other.dip.size)
    if (result==0) 
      result = (@base <=> other.base)
    end

    return result
  end
end

class Graph 
  def initialize(nodes)
    @nodes = nodes
    @edges = Bool_Matrix.new(nodes.size)

    @name_to_idx = Hash.new
    @nodes.each_with_index do |name, idx| 
      @name_to_idx[name]=idx
    end
  end

  def new_edge(from, to)
    from_idx = @name_to_idx[from]
    if from_idx.nil?
      $stderr.puts "Warning: Unknown FROM node '#{from}'" 
      return
    end

    to_idx = @name_to_idx[to]
    if to_idx.nil?
      $stderr.puts "Warning: Unknown TO node '#{to}' (<- #{from})" 
      return
    end

    @edges[from_idx,to_idx]=true
  end

  def closure(with_identity=false)
    base=@edges
    eye=Bool_Matrix.new(@nodes.size)
    eye.eye!

    empty=Bool_Matrix.new(@nodes.size)

    if (with_identity)
      base += eye
    end

    @nodes.size.times do |n|
      $stderr.puts n
      old = base
      base = (base+eye)*@edges
      break if base == old
    end

    result = Graph.new(@nodes)

    @nodes.size.times do |row|
      @nodes.size.times do |col|
	result.new_edge(@nodes[row],@nodes[col]) if base[row,col]
      end
    end

    return result
  end

  def print(ori_paths=nil)
    tmp = Array.new
    @nodes.size.times do |row|
      neigh=Array.new

      @nodes.size.times do |col|
	neigh << @nodes[col] if @edges[row,col]
      end

      x = @nodes[row]
      unless (ori_paths.nil? or ori_paths[x].nil?)
	x = ori_paths[x]
      end
      tmp << Dati.new(x, neigh);
    end

    tmp.sort!
    tmp.each do |x|  
      puts "#{x.base} : #{x.dip.join(',')}"
    end
  end

  def to_dot 
    result = "digraph pippo {\n"

    @nodes.size.times do |row|
      from = @nodes[row].tr('-', '_')
      neigh=Array.new
      neigh << from
      @nodes.size.times do |col|
	neigh << @nodes[col].tr('-', '_') if @edges[row,col]
      end

      result += neigh.join(' -> ') + ";\n"
    end
    
    result += "}"
    return result
  end

  attr_reader :edges
end


def to_filename(x)
  return x.downcase.tr('.', '-')
end

$ignore = ['private', 'null', 'system', 'network', 'interfaces', 'text_io',
  'dome_core_attrs', 'unicode_ces_utf8', 'schema_dom_readers',
  'dom_core', 'dom_core_documents', 'dom_readers', 'sax_readers',
  'schema_schema_grammar']

def process(filename)
  base = File.basename(filename, '.ads');
  base = File.basename(base, '.adb');
  dipendenze = Array.new

  File.open(filename) do |stream|
    stream.each do |line|
      if (line =~ /--/)
	line = $`
      end

      next if line =~ /limited +with/

      if (line =~ /with +([a-zA-Z0-9._]+) *;/) 
	withed = $1
	if ((not $ignore.include?(withed.downcase)) && 
	    (not withed.downcase =~ /(gnat|system|ada)\./))
	  dipendenze << to_filename(withed)
	end
      end
    end
  end

  return [base, dipendenze]
end

dati = Hash.new
ori_paths = Hash.new
ARGV.each do |filename|
  base, dip = process(filename)
  if dati[base] 
    dati[base] += dip
  else
    dati[base] = dip
    ori_paths[base] = filename
  end
end

graph = Graph.new(dati.keys)
risultati = Array.new

dati.each do |base, dip|
  risultati << Dati.new(base, dip)
  dip.each do |to|
    graph.new_edge(base, to)
  end
end

empty=Bool_Matrix.new(dati.keys.size);
p empty==graph.edges


risultati.sort!
risultati.each {|x|  puts "#{x.base} : #{x.dip.join(',')}"}

closure = graph.closure
