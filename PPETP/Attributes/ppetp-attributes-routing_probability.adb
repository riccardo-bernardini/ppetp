with Ada.Streams;			use Ada.Streams;
with PPETP.Attributes;			use PPETP.Attributes;
with Network_Utilities;			use Network_Utilities;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;
with byte_arrays;			use byte_arrays;
with Interfaces;


package body PPETP.Attributes.Routing_Probability is

   ------------------
   -- Parsing_Data --
   ------------------
   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class is

   begin

      return Access_Attribute_Class'( new ROUTING_PROBABILITY_Attribute'(Attr_Type => ROUTING_PROBABILITY_Attribute_Index,
                                                                         Num       => Data(1),
                                                                         Den       => Data(2)));
   end Parsing_Data;


   -------------------
   -- Building_Data --
   -------------------
   function Building_Data(Object : ROUTING_PROBABILITY_Attribute) return byte_array is
      Payload : Byte_Array(1..2);
   begin
      Payload(1) := Object.Num;
      Payload(2) := Object.Den;

      return Payload;
   end Building_Data;



   --------------------
   -- Set_Attributes --
   --------------------
   procedure Set_Attribute(Object : in out ROUTING_PROBABILITY_Attribute;
                           Num:     in     byte_arrays.Byte;
                           Den:     in     byte_arrays.Byte) is
   begin
      Object.Num := Num;
      Object.Den := Den;
   end Set_Attribute;


   --------------------
   -- Get_Attributes --
   --------------------
  procedure Get_Attribute(Object :    in out ROUTING_PROBABILITY_Attribute;
                           Attr_Type:    out Attributes_Index_Type;
                           Num:          out byte_arrays.Byte;
                           Den:          out byte_arrays.Byte) is
   begin
      Attr_Type  := Object.Attr_Type;
      Num        := Object.Num;
      Den        := Object.Den;
   end Get_Attribute;

   --------------------
   -- Get_Attributes --
   --------------------
   procedure Get_Attribute(Object :    in out ROUTING_PROBABILITY_Attribute;
                           Num:           out byte_arrays.Byte;
                           Den:           out byte_arrays.Byte) is
   begin
      Num        := Object.Num;
      Den        := Object.Den;
   end Get_Attribute;


begin

   PPETP.Attributes.Register(Index    => ROUTING_PROBABILITY_Attribute_Index,
                             Callback => Parsing_Data'Access);

end PPETP.Attributes.Routing_Probability;
