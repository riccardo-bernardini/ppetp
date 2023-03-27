with Network;		use Network;
with byte_arrays;	use byte_arrays;
with Ada.Streams;	use Ada.Streams;

package PPETP.Attributes.Peer_Credential is

   PEER_CREDENTIAL_Attribute_Index : constant Attributes_Index_Type := 2;


   type PEER_CREDENTIAL_Attribute (Size: Byte_Array_Offset) is
     new Root_Attributes with private;

   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class;
   function Building_Data(Object: PEER_CREDENTIAL_Attribute) return byte_array;

   function Get_Data_Length(Object : PEER_CREDENTIAL_Attribute) return Natural;

   procedure Get_Attribute(Object :   in out PEER_CREDENTIAL_Attribute;
                           Attr_Type:    out Attributes_Index_Type;
                           Data:         out Byte_Array);

   -- Data must have the same length of the object.data
   procedure Set_Attribute(Object :   in out PEER_CREDENTIAL_Attribute;
                           Data:      in     Byte_Array);

private

   type PEER_CREDENTIAL_Attribute(Size: Byte_Array_Offset) is new Root_Attributes with
      record
         Data: Byte_Array(1..Size);
      end record;

end PPETP.Attributes.Peer_Credential;
