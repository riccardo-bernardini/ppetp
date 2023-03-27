with Ada.Streams;			use Ada.Streams;
with PPETP.Attributes;			use PPETP.Attributes;
with Network_Utilities;			use Network_Utilities;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;
with byte_arrays;			use byte_arrays;
with Interfaces;


package body PPETP.Attributes.Peer_Credential is

   ------------------
   -- Parsing_Data --
   ------------------
   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class is

   begin

      return Access_Attribute_Class'( new PEER_CREDENTIAL_Attribute'(Size      => Data'Length,
                                                                     Attr_Type => PEER_CREDENTIAL_Attribute_Index,
                                                                     Data      => Data));
   end Parsing_Data;

   -------------------
   -- Building_Data --
   -------------------
   function Building_Data(Object: PEER_CREDENTIAL_Attribute) return byte_array is
   begin
      return Object.Data;
   end  Building_Data;


   ---------------------
   -- Get_Data_Length --
   ---------------------
   function Get_Data_Length(Object : PEER_CREDENTIAL_Attribute) return Natural is
   begin
      return Object.Data'Length;
   end Get_Data_Length;


   --------------------
   -- Get_Attributes --
   --------------------
   procedure Get_Attribute(Object :    in out PEER_CREDENTIAL_Attribute;
                           Attr_Type:     out Attributes_Index_Type;
                           Data:          out Byte_Array) is
   begin
      Attr_Type  := Object.Attr_Type;
      Data       := Object.Data;
   end Get_Attribute;


   -------------------
   -- Set_Attribute --
   -------------------
   procedure Set_Attribute(Object :   in out PEER_CREDENTIAL_Attribute;
                           Data:      in     Byte_Array) is
   begin
      Object.Data := Data;
   end Set_Attribute;



begin

   PPETP.Attributes.Register(Index    => PEER_CREDENTIAL_Attribute_Index,
                             Callback => Parsing_Data'Access);

end PPETP.Attributes.Peer_Credential;
