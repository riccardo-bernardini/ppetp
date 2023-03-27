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
   type Packet_Priority is mod 2**4;
   type Packet_Class    is mod 2**4;

   type Packet_Network_Timestamp is
     new Basic_Packet_Timestamp with null record;

   type Network_Packet(N_Components : Natural) is
      record
         Class    : Packet_Class;
         Priority : Packet_Priority;
         ID       : Packet_Network_Timestamp;
         Payload  : Network_Crumb_Array (1..N_Components);
      end record;

   function Packet_To_Bytes (Input : Network_Packet)
                            return Stream_Element_Array;

   function Bytes_To_Packet (Input : Stream_Element_Array)
                            return Network_Packet;
end Packets.Network;
