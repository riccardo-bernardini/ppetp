#!/bin/sh
exec ruby  -x  $0 "$@";
#! ruby

#
# This Ruby script reads (from stdin) the file produced by GNAT XREF and 
# write (to stdout) a DOT description that represents the dependence 
# graph.  The result can be processed by dot to produce a graphical
# representation.
#
# If option "--srcdir" is given, the script searchs in the source
# files a comment string of type 
#
#   -- Status: <tested>
#
# where "tested" can be replaced by other status values.  Currently
# recognized status values are "broken", "untested", "tested", 
# "fully tested", "formally proved".  The status value will be used
# to determine the style of the node associated with the file.  
# See entry 'status_to_style_map' in $configuration at the beginning
# of the main program.  The style value is case insensitive (e.g.,
# "TESTED", "teSTed" and "testeD" are equivalent).
#
# Currently the map status -> style is "hardwired" in the script.
# In a future I plan to make it configurable via a config file.
#

require 'find'
require 'getoptlong'

class Dependency_Entry
  def initialize(name, declared_in, used_by=[])
    @name = name
    @declared_in = declared_in
    @used_by = used_by
  end

  def new_user(user)
    if (user.is_a?(Array))
      @used_by += user
    elsif (user.is_a?(String))
      @used_by << user
    else
      raise "User must be array or string"
    end
  end

  def <=>(other)
    self.stringify <=> other.stringify
  end

  attr_reader :name, :declared_in, :used_by


  def stringify
    return "#{name}:#{declared_in}"
  end
end

class Dependencies_Table
  def initialize
    @table = Hash.new
  end

  def new_dependency(dependency)
    idx = dependency.stringify
    if (@table[idx].nil?)
      @table[idx] = dependency
    else
      @table[idx].new_user(dependency.used_by)
    end
  end

  def to_array
    return @table.values
  end
end

def get_line_type(line)
  if (line[0..0] != ' ')
    return :symbol_line
  elsif (line[9..-1] =~ /^[^ ]+\.ad[sb]/)
    return :field_line
  else
    return :continuation_line
  end
end

def parse_line(line)
  line_type = get_line_type(line)
  # $stderr.puts(line_type.inspect)
  data = Array.new
    case line_type
    when :symbol_line
      # -- do nothing
    when :continuation_line
      # -- do nothing
    when :field_line
      field_name = line[2..8].strip
      file_name, *ignored = line[9..-1].split
      
      raise "Funny filename in '#{line}'  at #{$stdin.lineno}" unless 
	file_name =~ /^(.*)\.([^.]*)$/

      file_name_no_ext = $1

      data = [field_name, file_name, file_name_no_ext]
    end

  return [line_type, data]
end

def parse_single_entry(input, line)
  entry_name, *ignore = line.split
  declared_in = nil
  body_in = nil
  used_by = []


  while line = input.gets
    line_type, data = parse_line(line)

    case line_type
    when :symbol_line
      break
    when :continuation_line
      next
    when :field_line
      field_name, file_name, file_name_no_ext = data

      case field_name.upcase
      when "DECL:"
	raise "Double DECL for #{entry_name} at #{input.lineno}" unless 
	  declared_in.nil?

	declared_in = file_name_no_ext
      when "BODY:"
	raise "Double BODY for #{entry_name} at #{input.lineno}" unless 
	  body_in.nil?

	body_in = file_name_no_ext
      when "REF:"
	used_by << file_name_no_ext
      end
    end
  end
  raise "No DECL for #{entry_name} at #{input.lineno}" if declared_in.nil?
  used_by = used_by.sort.uniq
  
  return [Dependency_Entry.new(entry_name, declared_in, used_by), line]
end

def parse_xref_file(input)
  dep_table = Dependencies_Table.new

  line = input.gets
  while (not line.nil?)
    this_entry, line = parse_single_entry(input, line)
    dep_table.new_dependency(this_entry) unless this_entry.nil?
  end

  return dep_table
end

def remove_system_library_nodes(entries, ignore_list)
  entries.each_index do |idx|
    if (entries[idx].declared_in =~ /^[a-z]-/ ||
	ignore_list.include?(entries[idx].declared_in))
      entries[idx] = nil
    end
  end

  return entries.compact
end

def to_node_name(s)
  return s.gsub(/[^a-zA-Z0-9]/, '_')
end

Node = Struct.new('Node', 'filename', 'style')
class Node
  def <=>(other)
    return filename <=> other.filename
  end
end


Edge = Struct.new('Edge', 'from', 'to')
class Edge
  def <=>(other)
    return "#{from}:#{to}" <=> "#{other.from}:#{other.to}"
  end
end

def all_nodes(entries)
  nodes = []
  entries.each do |entry|
    nodes << Node.new(entry.declared_in, nil)
    nodes += entry.used_by.map { |user|  Node.new(user, nil) }
  end

  return nodes.sort.uniq
end

def extract_edges(entries)
  edges = Array.new

  entries.each do |node|
    decl_node = to_node_name(node.declared_in)

    node.used_by.each do |user|
      usr_node  = to_node_name(user)

      # Skip self-dependences
      next if  usr_node == decl_node

      edges << Edge.new(usr_node, decl_node)
    end
  end

  return edges.sort.uniq
end

class Source_File_Info
  def initialize(attributes=nil)
    if (attributes.nil?)
      @attr_table = Hash.new
    elsif (attributes.is_a?(Hash))
      @attr_table = attributes
    else
      raise "Attributes must be a Hash"
    end
  end

  def status
    return @attr_table["status"]
  end

  def type
    return @attr_table["type"]
  end

  def full_path
    return @attr_table["full_path"]
  end

  def []=(attr_name, attr_val)
    @attr_table[attr_name]=attr_val
  end

  def [](attr_name)
    return @attr_table[attr_name]
  end
end

def get_source_files(srcdir)
  status_regexp, status_value_index = make_comment_regexp("Status")
  type_regexp,   type_value_index   = make_comment_regexp("Type")

  regexp_to_attr = [ 
    [status_regexp, status_value_index, "status"],
    [type_regexp, type_value_index, "type"]]

  result = Hash.new

  Find.find(srcdir) do |filename|
    full_path = File.expand_path(filename)
    true_name = File.basename(full_path)
    if (true_name[0..0] == '.' && true_name != '.')
      Find.prune
    elsif (true_name =~ /\.ads$/)
      # p [true_name, File.basename(true_name, '.ads')]
      new_entry = Source_File_Info.new("full_path" => full_path)

      File.open(full_path) do |input|
	while (line = input.gets)
	  regexp_to_attr.each { |data|
	    if (line =~ data[0])
	      new_entry[data[2]] = Regexp.last_match[data[1]]
	    end
	  }
	end
      end

      idx_name = File.basename(true_name, '.ads')
      result[idx_name] = new_entry
    else
      # puts "#{filename} ignorato"
    end
  end

  return result
end

def print_dot_file(nodes, entries, graph_name = 'pippo')
  print "digraph #{graph_name} {\n"

  nodes.each do |node|
    print "#{to_node_name(node.filename)}[label=\"#{node.filename}\""
    print ",#{node.style}" if (node.style)
    print "];\n"
  end

  extract_edges(entries).each do |edge|
    print "#{edge.from} -> #{edge.to};\n"
  end

  print "}\n";
end

def make_comment_regexp(field_name)
  result  = $configuration['special_comment_head_regexp'];
  result += $configuration['special_comment_name_regexp'] % field_name;
  result += $configuration['special_comment_value_regexp'];

  return [Regexp.new(result), $configuration['special_comment_value_index']]
end

def get_source_attribute(filename, source_files, attr_name)
  source_info = source_files[filename]

  if source_info.nil?
    return nil 
  else
    return source_info[attr_name]
  end

  # status_regexp, value_index = make_comment_regexp("Status")
  # 
  # status = nil
  # File.open(source) do |input|
  #   while line = input.gets
  #     if (line =~ status_regexp)
  # 	status = Regexp.last_match[value_index]
  # 	break
  #     end
  #   end
  # end
end

def get_status(filename, source_files)
  return get_source_attribute(filename, source_files, "status")
end

def status_to_style(status)
  unknown_status_style = $configuration['unknown_status_style']
  status_to_style_map  = $configuration['status_to_style_map']

  if status.is_a?(String)
    status = status.upcase.split.join(' ')
  end

  if status_to_style_map.include?(status)
    style = status_to_style_map[status]
    while (style and style[0..0]=='%')
      style = status_to_style_map[style[1..-1]]
    end

    if (style.nil?)
      return unknown_status_style
    else
      return style
    end
  else
    return unknown_status_style
  end
end

def join_styles(*styles)
  styles = styles.compact
  if styles.empty?
    return nil 
  else
    return styles.join(',')
  end
end

def type_to_style(file_type)
  return nil if file_type.nil?

  $stderr.puts file_type
  case file_type.upcase
  when "GENERIC"
    return 'shape="diamond"'
  when /^INSTANCE/
    return 'shape="trapezium"'
  else
    return nil
  end
end

def apply_status_style(nodes, srcdir)
  return if srcdir.nil?
  source_files = get_source_files(srcdir)

  nodes.each_index do |idx|
    filename = nodes[idx].filename
    status = get_source_attribute(filename, source_files, "status")
    type   = get_source_attribute(filename, source_files, "type")
    nodes[idx].style = join_styles(status_to_style(status),
				   type_to_style(type))
  end
end

###
### MAIN
###

srcdir = nil
config_file = nil

$configuration = {
  'ignore_list'          => ['ada', 'gnat', 'text_io', 'interfac'],
  'unknown_status_style' => 'fontcolor="magenta"',
  'status_to_style_map'  => { 
    nil => "%UNTESTED",
    'UNTESTED' => 'color=blue,fontcolor="red"',
    'BROKEN'   => 'style="filled",fontcolor="blue",fillcolor="red"',
    'TESTED'   => 'color=blue,fontcolor="green"',
    'FULLY TESTED'    => 'fontcolor="green",color="green"',
    'FORMALLY PROVED' => 'style="filled",fontcolor="black",fillcolor="green"'
  },

  'special_comment_head_regexp'  => "^-- +",
  'special_comment_name_regexp'  => "%s *:",
  'special_comment_value_regexp' => " *<([^>]+)>",
  'special_comment_value_index'  => 1
}


opts = GetoptLong.new(["--srcdir", "-s", GetoptLong::REQUIRED_ARGUMENT],
		      ["--config", "-c", GetoptLong::REQUIRED_ARGUMENT])

opts.each do |opt, arg|
  case opt
  when "--srcdir"
    srcdir = arg
  when "--config"
    config_file = arg
    $stderr.puts "Warning: --config option currently ignored"
  else
    raise "Unhandled option #{opt}"
  end
end


entries = parse_xref_file($stdin).to_array
entries = remove_system_library_nodes(entries, $configuration['ignore_list'])
nodes   = all_nodes(entries)

if (srcdir)
  apply_status_style(nodes, srcdir)
end

print_dot_file(nodes, entries)
