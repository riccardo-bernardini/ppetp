with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

package Packet_Buffer is
   subtype Packet_Buf is Stream_Element_Array;
   type Packet_Buf_Pt is access Packet_Buf;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Packet_Buf,
                                     Name   => Packet_Buf_Pt);
end packet_buffer;
