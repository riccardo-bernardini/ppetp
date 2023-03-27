with Network;		use Network;
with byte_arrays;	use byte_arrays;
with Ada.Streams;	use Ada.Streams;

package PPETP.Attributes.Nat_Parameter is

   NAT_PARAMETER_Attribute_Index : constant Attributes_Index_Type := 4;


   type NAT_PARAMETER_Attribute (<>) is
     new Root_Attributes with private;

   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class;
   function Building_Data(Object: NAT_PARAMETER_Attribute) return byte_array;


   function Get_Data_Length(Object : NAT_PARAMETER_Attribute) return Natural;

   procedure Get_Attribute(Object :   in out NAT_PARAMETER_Attribute;
                           Attr_Type:    out Attributes_Index_Type;
                           Data:         out Byte_Array);

private

   type NAT_PARAMETER_Attribute(Size: Stream_Element_Offset) is new Root_Attributes with
      record
         Data: Byte_Array(1..Size);
      end record;

end PPETP.Attributes.Nat_Parameter;
