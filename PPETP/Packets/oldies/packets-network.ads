with Ada.Streams;
use  Ada.Streams;

with Network;

package Packets.Network is
   -- "Raw" packet (just a sequence of bytes) as read from network --
   ------------------------------------------------------------------

   type Raw_Packet is access Streams.Stream_Element_Array;
   -- type Raw_Packet_Pt is access Raw_Packet;

   function To_Stream_Array(Packet : Raw_Packet)
                            return Streams.Stream_Element_Array;

   function To_Raw_Packet (Data :  Streams.Stream_Element_Array)
                           return Raw_Packet;


   procedure Free (Packet : in out Raw_Packet);
end Packets.Network;
