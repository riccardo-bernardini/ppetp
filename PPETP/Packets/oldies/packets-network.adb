with Ada.Unchecked_Deallocation;

package body Packets.Network is
   function To_Stream_Array (Packet : Raw_Packet)
                            return Streams.Stream_Element_Array is
   begin
      return Streams.Stream_Element_Array(Packet.all);
   end To_Stream_Array;
   pragma Inline(To_Stream_Array);

   function To_Raw_Packet (Data :  Streams.Stream_Element_Array)
                           return Raw_Packet is
   begin
      return new Streams.Stream_Element_Array'(Data);
   end To_Raw_Packet;

   procedure Free (Packet : in out Raw_Packet) is
      procedure Deallocate_Raw_Packet is
         new Ada.Unchecked_Deallocation(Object => Streams.Stream_Element_Array,
                                        Name   => Raw_Packet);
   begin
      Deallocate_Raw_Packet (Packet);
   end Free;
end Packets.Network;
