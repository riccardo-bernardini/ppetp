--                              -*- Mode: Ada -*-
--  Filename        : sent_db.ads
--  Description     : Handling the expiration times of sent commands
--  Author          :
--  Created On      : Wed Oct 22 15:41:52 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <FULLY TESTED>

--
-- ===================
-- == What is this? ==
-- ===================
--
-- This package provides types and methods to handle the timeout
-- expiration
--
-- ---------------------------
-- -- ...and that means...? --
-- ---------------------------
--
-- In PPETP the peer exchange "command packets". Most of command
-- packets require an acknowledge and if the acknowledge is not
-- received after a given amount of time, the packet is sent
-- again.  This package provides a type Table which store
-- informations about sent packets and their ACK timeout.
-- Table can also be queried in order to know the timeout
-- of the earliest packet or to get the information about
-- expired packets.
--

with Ada.Calendar;       use Ada.Calendar;
with Ada.Streams;        use Ada.Streams;
with PPETP_Mailboxes;	 use PPETP_Mailboxes;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with PPETP;

with PPETP_Mail;	use PPETP_Mail;
with Network;

package Sent_DB is
   type Stream_Array_Pt is access Stream_Element_Array;

   -- Packet information stored in the table
   type Packet_Info is
      record
         Data         : Stream_Array_Pt;         -- Actual packet
         Target       : Network.Sock_Addr_Type;  -- Remote peer address
         Send_Port    : Network.Port_Type;
         Sequence_Num : PPETP.Command_Sequence_Number;   -- Packet Sequence_Number
         Reply_Mbox   : Mailbox_Access;          -- Mailbox for ACK
      end record;

   -- Special value
   No_Info : constant Packet_Info;

   type Table is tagged private;

   -- Set the timeout interval
   procedure Set_Timeout_Interval (T : in out Table; X : Duration);

   procedure Set_Routed_Timeout_Interval (T : in out Table; X : Duration);

   -- Set the maximum number of times we try to send a packet
   -- before giving up
   procedure Set_Max_Trials (T : in out Table; N : Natural);

   -- Store in the table information about a just sent
   -- packet.  This procedure is to be called each time the
   -- packet is sent.  If the packet has been sent too many
   -- times, Info is discarded and False is sent to the packet
   -- mailbox.
   procedure Packet_Sent (Tbl    : in out Table;
                          Info   : in     Packet_Info;
                          Routed : in     Boolean);

   -- Get the value of the first timeout.  It return
   -- Clock + Max_Wait if the table is empty or the
   -- earliest timeout is after Clock + Max_Wait.
   function Earliest_Timeout (Tbl      : Table;
                              Max_Wait : Duration) return Time;

   -- Get the value of the first timeout.  Raise Empty_Table
   -- if no timeout is scheduled.
   function Earliest_Timeout (Tbl : Table) return Time;
   Empty_Table : exception;

   -- Return true if the table is empty.
   function Is_Empty (Tbl : Table) return Boolean;

   -- Extract from the table the first expired packet.  Return
   -- No_Info in Info if no expired packet exists.
   procedure Next_Expired (Tbl  : in out Table;
                           Info :    out Packet_Info);

   -- Get the informations associated with packet with
   -- a given timestamp.  Returns No_Info if no packet
   -- with the given timestamp exists.
   function Find (Tbl          : Table;
                  Sequence_Num : PPETP.Command_Sequence_Number) return Packet_Info;


   -- Say to the table that we are done with the packet with the
   -- given timestamp.  Value of Result will be written to
   -- the mailbox associated with the packet.
   procedure Packet_Done (Container    : in out Table;
                          Sequence_Num : in     PPETP.Command_Sequence_Number;
                          Result       : in     PPETP_Mail_Ack_Type);

private
   No_Info : constant Packet_Info := (Data         => null,
                                      Target       => Network.No_Sock_Addr,
                                      Send_Port    => Network.No_Port,
                                      Sequence_Num => <>,
                                      Reply_Mbox   => null);
   --
   -- We need to access packet info both according to packet
   -- timestamp and timeout.  In order to have fast access in both cases
   -- we keep two structures:
   --
   --    * a "timeout ordered map" whose entries are pairs
   --      (timeout, timestamp) ordered in lexicographic order.
   --      The first element in the timeout map corresponds to
   --      the packet with the earliest timeout
   --
   --    * a packet map having timestamps as key and packet_info
   --      as entries (actually, packet_info is only part of the
   --      map entry.  The actual entry contains also other
   --      informations, e.g.,  how many retry we can do)
   --
   -- Given an entry in a map we can easily find the corresponding
   -- entry in the other map, indeed,
   --
   --    * If we have an entry of the first map, we can access
   --      the second one using the timestamp.
   --
   --    * If we have an entry of the second map, we know timestamp
   --      and timeout and we can access the first map
   --

   --
   -- Stuff for the timeout map
   --

   type Timeout_Info is
      record
         Timeout      : Time;
         Sequence_Num : PPETP.Command_Sequence_Number;
      end record;

   function "<" (Left, Right : Timeout_Info) return Boolean;

   package Timeout_Map is
     new Ada.Containers.Ordered_Sets (Element_Type => Timeout_Info);

   --
   -- Stuff for the packet_info map
   --

   use type PPETP.Command_Sequence_Number;

   -- Actually entry of the map
   type Sent_Packet_Info is
      record
         Info       : Packet_Info;  -- Received by the user
         Remaining_Trials : Natural;
         -- Max_Trials : Natural;
         Timeout    : Time;
      end record;

   type Packet_Info_Access is access Sent_Packet_Info;


   package PKT_Map is
     new Ada.Containers.Ordered_Maps (Key_Type     => PPETP.Command_Sequence_Number,
                                      Element_Type => Packet_Info_Access);

   --
   -- Finally, we can define the table type.
   --
   type Table is tagged
      record
         Packets                : PKT_Map.Map;      -- packet_info map
         Timeouts               : Timeout_Map.Set;  -- timeout map
         Timeout_Interval       : Duration := 30.0;
         Timeout_Routed_Interval: Duration := 30.0;
         Max_Trials             : Natural  := 10;
      end record;
end Sent_DB;
