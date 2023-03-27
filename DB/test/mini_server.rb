#!/bin/sh
exec ruby  -I.. -I../../LDS -x  $0 "$@";
#! ruby

require 'internal_state'
require 'thread'

Thread.abort_on_exception=true

State_Server = Internal_State::Server.new
State_Server.listen_to_port(54321)

sleep

