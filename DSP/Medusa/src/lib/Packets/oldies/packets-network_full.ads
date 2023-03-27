--                              -*- Mode: Ada -*-
--  Filename        : packets-network.ads
--  Description     : Definition of a network packet
--  Author          : Riccardo Bernardini
--  Created On      : Fri Jun 27 16:32:50 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Timestamps;
use  Timestamps;

with Packets.Network_Crumbs;
use  Packets.Network_Crumbs;

package Packets.Network is
   -- Header_Length       : constant Positive := 4; -- bytes
   -- N_Packet_Field_Size : constant Integer  := 2;  -- bits
   -- subtype Header is Byte_Array(1..Header_Length);
   -- subtype Sub_Packets_Count is Integer range 1..2**N_Packet_Field_Size;
   --
   -- Sub_Header_Length   : constant Integer  := 2; -- bytes
   -- Class_Field_Size    : constant Integer  := 2; -- bits
   -- Max_Sub_Packet_Size : constant Positive :=
   --   2**(8*Sub_Header_Length-Class_Field_Size);
   --
   -- subtype Sub_Header is Byte_Array(1..Sub_Header_Length);

   -- type Network_Packet is new External_Packet with null record;

   type Packet_Priority is mod 2**4;
   type Packet_Class    is mod 2**4;
   type Component_Counter is new Integer 1..2**4;

   type Packet_Network_Timestamp is
     new Basic_Packet_Timestamp with null record;

   type Network_Info is
      record
         Network_Class : Packet_Class;
         Priority      : Priority;
         Stream_ID     : Stream_ID_Type;
      end record;

   type Network_Packet(N_Components : Natural) is
      record
         Info    : Packet_Network_Info;
         ID      : Packet_Network_Timestamp;
         Payload : Network_Crumb_Array (1..N_Components);
      end record;

   function To_Bytes (Input : Network_Packet) return Byte_Array;
   function Parse_Bytes (Input : Byte_Array) return Network_Packet;
end Packets.Network;
