--                              -*- Mode: Ada -*-
--  Filename        : network_reader.ads
--  Description     : Definition of the task which receives data from peers
--  Author          : Riccardo Bernardini
--  Created On      : Fri Feb 15 11:52:44 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides the definition of a task Reader.  The duty
-- of a Reader is to receive packets from the network, convert them
-- to Input_Packet and enqueue them into the command queue read by
-- the "core" of the DSP module.
--
-- A Reader task exposes two entry points: Start and Finish
--
with GNAT.Sockets;

with Input_Data;

package Network_Readers is
   task type Reader is
      --
      -- In order to do its work a Reader needs
      --
      --   1. A source where the packets are read from
      --   2. The packet queue where to enqueue the read data
      --   3. The type of packet (if command or data)
      --
      entry Start(Source      : GNAT.Sockets.Socket_Type;
                  Destination : Input_Data.Command_Queue_Access;
                  Class       : Input_Data.Input_Packet_Class);

      --
      -- Called to terminate the task
      --
      entry Finish;
   end Reader;

   type Reader_Access is access Reader;
end Network_Readers;
