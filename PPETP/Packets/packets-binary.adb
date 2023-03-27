with  Ada.Unchecked_Deallocation;

package body Packets.Binary is
   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Byte_Array,
                                     Name   => Byte_Array_Pt);


   function Get (Packet : Binary_Packet;
                 Index  : Byte_Array_Offset)
                 return Byte is
   begin
      return Packet.Data_Buffer (Index);
   end Get;

   function Buffer (Packet : Binary_Packet)
                    return Byte_Array is
   begin
      return Packet.Data_Buffer.all;
   end Buffer;

   procedure Reserve (Packet : in out Binary_Packet;
                      Length : in     Byte_Array_Offset) is
   begin
      Free (Packet.Data_Buffer);
      Packet.Data_Buffer := new Byte_Array (1 .. Length);
   end Reserve;

   procedure Set (Packet : in out Binary_Packet;
                  Data   : in     Byte_Array) is
   begin
      Packet.Reserve (Data'Length);
      Packet.Data_Buffer.all := Data;
   end Set;

   procedure Set (Packet : in out Binary_Packet;
                  Index  : in     Byte_Array_Offset;
                  Data   : in     Byte) is
   begin
      Packet.Data_Buffer (Index) := Data;
   end Set;

   procedure Adjust (Object : in out Binary_Packet) is
      Tmp : Byte_Array_Pt :=
              new Byte_Array '(Object.Data_Buffer.all);
   begin
      Object.Data_Buffer := Tmp;
   end Adjust;

   procedure Finalize (Object : in out Binary_Packet) is
   begin
      Free (Object.Data_Buffer);
   end Finalize;
end Packets.Binary;
