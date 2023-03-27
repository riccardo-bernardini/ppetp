--                              -*- Mode: Ada -*-
--  Filename        : ppetp-targets.ads
--  Description     : Definition related to the targets
--  Author          : Riccardo Bernardini
--  Created On      : Tue Sep  9 11:50:54 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Network;
with Packets.Binary.Network;       use Packets.Binary.Network;
with Network_Packet_Queues;
with PPETP.Attributes;	use PPETP.Attributes;
--
-- This package provides the types related to targets in PPETP.
--

package PPETP.Targets is
   type Target is tagged private;

   Target_Ready     : exception;
   Target_Not_Ready : exception;
   Target_Closed    : exception;

   -- Create a new target connected to the given address
   function New_Target (Address : Network.Sock_Addr_Type;
                        Output_Queue: Network_Packet_Queues.Queue_Pt;
                        Puncturing : Access_Attribute_Class) return Target;

   -- Send a packet to the target
   procedure Send (To   : Target;
                   Item : Network_Packet;
                   Sequence_Num: Data_Sequence_Number);

   -- Declare the target ready
   procedure Switch_To_Ready (T : in out Target);

   -- Shutdown the target
   procedure Close (T : in out Target);

   -- Get the target address;
   function Address(T : in Target) return Network.Sock_Addr_Type;

   -- Print the target
   procedure Print(T       : in Target;
                   Tab_Num : in Natural);

private
   type Target_Status is (Ready, Not_Ready, Closed);

   type Target is tagged record
      Address : Network.Sock_Addr_Type;
      Output_Queue: Network_Packet_Queues.Queue_Pt;
      Status  : Target_Status := Not_Ready;
      Punct_Attr : Access_Attribute_Class;
   end record;
end PPETP.Targets;
