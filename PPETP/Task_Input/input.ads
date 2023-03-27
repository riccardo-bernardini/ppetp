--                              -*- Mode: Ada -*-
--  Filename        : input.ads
--  Description     : Definition of the task which receives data from peers
--  Author          : Riccardo Bernardini
--  Created On      : Fri Feb 15 11:52:44 2008
--  Last Modified By: Roberto Cesco Fabbro
--  Last Modified On: Mon, sep 14 2009
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides the definition of the task of Reader type
-- whose duty is to receive packets from remote peer, convert them in
-- an internal format  and store the result into the internal
-- Command_Queue.  Some types of packets (e.g., the Set_Default
-- and the Set_Port command packets) are processed by the task itself.
--
-- Beside the received packets the reader task can insert into the
-- command queue also
--
--   1) Timeout packet when a timeout occours
--   2) ACK request packet, used to Acknowledge to command packets
--      processed by the task itself.
--
with Network;		use Network;
with Common_Types;	use Common_Types;
with Command_Queues;
--with Profiles;		use Profiles;
with PPETP;		use PPETP;

with Peer_Manager;

package Input is
   --
   -- Type of reader task.  There can be two different types of reader
   -- tasks: data packet reader and control packet reader.  The
   -- former receives mainly data from other peers, the latter can
   -- receive only control packets.  Currently data reader can receive
   -- only command packets of type Reply_Port and Set_Default.
   --
--   type Pause_Type is ( STOP_PAUSE, START_PAUSE);
   type Input_Task_State is (Closed, Opened, Listening);




   task type Reader (Destination  : Command_Queues.Queue_Pt;
                     Input_Socket : Socket_Access;
                     Peer_Manag   : Peer_Manager.Peer_Manager_pt;
                     Inter_Proc_Socket : Socket_Access;
                     PeerID       : access constant PPETP.Peer_ID) is
      --
      -- In order to do its work a Reader needs
      --
      --   1. A source where the packets are read from
      --   2. The packet queue where to enqueue the read data
      --

      --
      -- A reader can be in the following states
      --
      --   1. Closed
      --   2. Open
      --   3. Listening
      --   4. Terminated
      --
      -- The corresponding state diagram is the following
      --
      --
      --                                  Stop
      --  Start ---> Closed ----------------------+
      --               |                          |
      --               | Open(Port)               |
      --               |                          |
      --               V                  Stop    |
      --       +---->Opened ----------------------+
      --       |       |                          |
      --       |       | Accept_From(Address)     |
      --       |       |                          |
      --       |       V                  Stop    |
      --       |   Listening ---------------------+
      --       |       |                          |
      --       |       | Close                    |
      --       |       |                          V
      --       +-------+                     Terminated
      --
      --

      --entry Open (Port : in out Network.Port_Type);
      entry Open;
      --
      -- Open a new listening port.  Returns in Port the number
      -- of the port actually open.  If the required port is
      -- busy, return Network.No_Port in Port.
      --



--      entry Pause(Action: Pause_Type);
      -- Action = STOP_PAUSE:  stop and un-bind the socket
      -- Action = START_PAUSE: start and re-bind the socket
      -- it act only on the fast-reader; the others task work again

      entry Close;
      --
      -- Stop listening.
      --

      entry Set_Timeout (Timeout : Duration);
      --
      -- Set the channel timout.  The task expect to receive at least
      -- one packet _from_the_accepted_source_ every Timeout seconds.
      -- If a timeout happen a Timeout packet is inserted into the
      -- queue Destination.
      --

      entry Change_In_Out_Socket(In_Out_Socket : Socket_Access);

      entry Start;

      entry Stop_Read;
      entry Start_Read;
      --
      -- the library is ready to analyse PPETP packets

      entry Stop;
      --
      -- Called to terminate the task
      --

      --entry Port(Task_Port: out Network.Port_Type);
      --
      -- Get the receiving port
      --
   end Reader;


   type Reader_Task is access Reader;

   procedure Finalize (X : in out Reader_Task);

end Input;
