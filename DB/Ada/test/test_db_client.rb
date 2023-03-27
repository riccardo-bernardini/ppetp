#!/usr/bin/env ruby

#
# This is a Ruby script used to test the Ada library for access
# to the DB server.  The test is carried out by simply writing
# some random variable to the server and reading them back
#

#
# Generate a random string with the char in 'chars' and with maximum
# length 'maxlen'
#
def random_string(chars, maxlen)
  len = rand(maxlen)+1
  result = ''
  len.times { idx=rand(chars.size); result += chars[idx..idx] }

  return result
end

port = 54321  # Port used by the DB server
n_times = 25  # N. of trials
n_ok    = 0   # N. of successful tests

alpha="abcdefghijklmnopqrstuvwxyz";  # Used for generating
digit="0123456789"                   # random variable names and
others=",<>/: -=_+)(*&^%$#!~@";      # values


#
# Start the DB server
#
pid = fork { exec("../../Ruby/mini_server.rb %d" % port) }
sleep 1  # give some time to the server 

# Kill the DB server at exit
at_exit { Process.kill("QUIT", pid) }

#
# Do the tests
#
n_times.times do
  # Generate a random variable name and value
  var=random_string(alpha, 1) + random_string(alpha+digit, 12);
  val=random_string(alpha+digit+others, 42);

  # Write the variable to the DB
  system("./mini_db_client #{port} set #{var} '#{val}'")

  # Read the variable back
  result=IO.popen("./mini_db_client #{port} get #{var}") do
    |stream| 
    stream.gets.chomp 
  end

  if (result == val)
    puts "OK"
    n_ok += 1
  else
    puts "FAIL: Wrote  '#{val}' in '#{var}' Read '#{result}'"
  end
end

puts "Passed #{n_ok} tests out of #{n_times}" 

