--                              -*- Mode: Ada -*-
--  Filename        : packets-binary-network.ads
--  Description     : Definition of network packets
--  Author          : Finta Tartaruga
--  Created On      : Tue Nov  4 21:50:06 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <FULLY TESTED> Nov  4 21:50:06 2008
--

with Ada.Streams;                 use Ada.Streams;
with Network;                     use Network;

package Packets.Binary.Network is
   type Network_Packet is new Binary_Packet with private;

   -- Create a new packet assigning to it a payload and
   -- (possibly) the address of its source
   function New_Packet (Data        : Byte_Array;
                        Remote_Addr : Sock_Addr_Type := No_Sock_Addr)
                        return Network_Packet;


   procedure Set_Peer (Packet      : in out Network_Packet;
                       Remote_Addr : in     Sock_Addr_Type);

   function Peer (Packet : Network_Packet) return Sock_Addr_Type;
private
   type Network_Packet is
     new Binary_Packet with
      record
         Remote_Peer : Sock_Addr_Type;
      end record;
end Packets.Binary.Network;
