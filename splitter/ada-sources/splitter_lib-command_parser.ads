--
-- *******************
-- ** What is this? **
-- *******************
--
-- This package provides a procedure that fills the Config variable  with
-- values read from the command line.  The syntax is of "named parameter"
-- type, that is, each command line parameter is expected to have the
-- following format
--
--          label ['=' value]
--
-- where "label" is any string without '='.
--
-- Currently the following parameters are defined
--
--     src=port   (mandatory)
--       Source UDP port to be used to received RTP data
--
--     ctl=port   (mandatory)
--       Control TCP port (on 127.0.0.1) used to communicate with
--       the Ruby counterpart
--
--     timeout=float   (optional)
--       Timeout in seconds before declaring an SSRC closed
--
-- In case of error Bad_Command is raised
--
with Splitter_Lib.Rtp;
with Splitter_Lib.BSD_Sockets;

package Splitter_Lib.Command_Parser is
   type Outbound_Connection is
      record
         Internal_Port : Natural;
         Remote_Host   : BSD_Sockets.Sock_Addr_Type;
      end record;

   No_Connection : constant Outbound_Connection :=
     (Internal_Port => 0,
      Remote_Host   => BSD_Sockets.No_Sock_Addr);

   type Config_Data is
      record
         Source_Port  : Natural;
         Control_Port : Natural;
         Ext_Address  : BSD_Sockets.Inet_Addr_Type;
         Timeout      : Duration;
         Ignored_SSRC : RTP.SSRC_Type;
         Target       : Outbound_Connection;
      end record;

   Bad_Command : exception;

   procedure Parse_Command_Line(Config : out Config_Data);
end Splitter_Lib.Command_Parser;
