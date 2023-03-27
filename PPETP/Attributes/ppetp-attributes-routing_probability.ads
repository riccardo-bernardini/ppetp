with Network;		use Network;
with byte_arrays;	use byte_arrays;
with Ada.Streams;	use Ada.Streams;

package PPETP.Attributes.Routing_Probability is

   ROUTING_PROBABILITY_Attribute_Index : constant Attributes_Index_Type := 5;


   type ROUTING_PROBABILITY_Attribute is
     new Root_Attributes with private;

   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class;
   function Building_Data(Object: ROUTING_PROBABILITY_Attribute) return byte_array;

   procedure Set_Attribute(Object : in out ROUTING_PROBABILITY_Attribute;
                           Num:     in     byte_arrays.Byte;
                           Den:     in     byte_arrays.Byte);

   procedure Get_Attribute(Object :   in out ROUTING_PROBABILITY_Attribute;
                           Attr_Type:    out Attributes_Index_Type;
                           Num:          out byte_arrays.Byte;
                           Den:          out byte_arrays.Byte);

   procedure Get_Attribute(Object :   in out ROUTING_PROBABILITY_Attribute;
                           Num:          out byte_arrays.Byte;
                           Den:          out byte_arrays.Byte);

private

   type ROUTING_PROBABILITY_Attribute is new Root_Attributes with
      record
         Num       : byte_arrays.Byte;
         Den       : byte_arrays.Byte;
      end record;

end PPETP.Attributes.Routing_Probability;
