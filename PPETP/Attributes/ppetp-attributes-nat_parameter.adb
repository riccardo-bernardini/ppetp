with Ada.Streams;			use Ada.Streams;
with PPETP.Attributes;			use PPETP.Attributes;
with Network_Utilities;			use Network_Utilities;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;
with byte_arrays;			use byte_arrays;
with Interfaces;

with Ada.Text_IO;			use Ada.Text_IO;

package body PPETP.Attributes.Nat_Parameter is

   ------------------
   -- Parsing_Data --
   ------------------
   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class is

   begin


      return Access_Attribute_Class'( new NAT_PARAMETER_Attribute'(Size      => Data'Length,
                                                                   Attr_Type => NAT_PARAMETER_Attribute_Index,
                                                                   Data      => Data));
   end Parsing_Data;

   -------------------
   -- Building_Data --
   -------------------
   function Building_Data(Object: NAT_PARAMETER_Attribute) return byte_array is
   begin

      return Object.Data;
   end  Building_Data;


   ---------------------
   -- Get_Data_Length --
   ---------------------
   function Get_Data_Length(Object : NAT_PARAMETER_Attribute) return Natural is
   begin
      return Object.Data'Length;
   end Get_Data_Length;


   --------------------
   -- Get_Attributes --
   --------------------
   procedure Get_Attribute(Object :    in out NAT_PARAMETER_Attribute;
                           Attr_Type:     out Attributes_Index_Type;
                           Data:          out Byte_Array) is
   begin
      Attr_Type  := Object.Attr_Type;
      Data       := Object.Data;
   end Get_Attribute;

begin

   PPETP.Attributes.Register(Index    => NAT_PARAMETER_Attribute_Index,
                             Callback => Parsing_Data'Access);

end PPETP.Attributes.Nat_Parameter;
