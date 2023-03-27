#!/usr/bin/env ruby

def print_block(output, label, field_size, tabbing, value)
  data = Array.new;
  for i in 0...value.size/2
    data << "16##{value[2*i..2*i+1]}#"
  end

  output.print(" " * tabbing)
  label = label + (" " * (field_size-label.size))
  output.print("#{label} => (")
  output.print(data.join(", "))
  output.print(")")
end

def single_line_to_ada(output, line)
  key, clear_text, cypher_text = line.split
  if ($n_entries > 1)
    output.print(",\n    ")
  end

  field_size = "Cypher_Text".size
  tabbing    = "000 => ".size + 5
  output.print("#{'%3d' % $n_entries} => (");
  print_block(output, "Key", field_size, 0, key)
  output.print(",\n")
  print_block(output, "Clear_Text", field_size, tabbing, clear_text)
  output.print(",\n")
  print_block(output, "Cypher_Text", field_size, tabbing, cypher_text)
  output.print(")")

  $n_entries += 1
end

file_in  = "blowfish_test_vectors.txt"
file_out = "blowfish_test_vectors.ads"

Header = ["package Blowfish_Test_Vectors is",
  "  type Blowfish_Vector is",
  "    record",
  "       Key         : Blowfish_Block;",
  "       Clear_Text  : Blowfish_Block;",
  "       Cypher_Text : Blowfish_Block;",
  "    end record;",
  "",
  "   Test_Vectors : Blowfish_Vector :=",
  "   ("
];

Trailer = ["     );", "end Blowfish_Test_Vectors;"];

$n_entries = 1;

File.open(file_in) do |input|
  while true
    line=input.gets
    raise "Wrong format" if line.nil?
    break if (line.chomp == '--')
  end

  File.open(file_out, 'w') do |output|
    output.print(Header.join("\n"))

    while true
      line=input.gets
      raise "Wrong format" if line.nil?
      break if (line.chomp == '--')
      single_line_to_ada(output, line)
    end

    output.puts(Trailer.join("\n"))
  end
end


