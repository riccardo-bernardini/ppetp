with Network;		use Network;
with byte_arrays;	use byte_arrays;
with PPETP;		use PPETP;

package PPETP.Attributes.Ack_Target is

   ACK_TARGET_Attribute_Index : constant Attributes_Index_Type := 6;


   type ACK_TARGET_Attribute is
     new Root_Attributes with private;

   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class;
   function Building_Data(Object: ACK_TARGET_Attribute) return byte_array;

   procedure Set_Attribute(Object :    in out ACK_TARGET_Attribute;
                           Address:    in     Sock_Addr_Type;
                           T_Protocol: in     byte_arrays.Byte;
                           PeerID:     in     Peer_ID);

   procedure Get_Attribute(Object :    in out ACK_TARGET_Attribute;
                           Attr_Type:     out Attributes_Index_Type;
                           Address:       out Sock_Addr_Type;
                           T_Protocol:    out byte_arrays.Byte;
                           PeerID:        out Peer_ID);

   function Get_Address(Object : ACK_TARGET_Attribute) return Sock_Addr_Type;

   function Get_PeerID(Object : ACK_TARGET_Attribute) return Peer_ID;

   procedure Image(Object : ACK_TARGET_Attribute);

private

   type ACK_TARGET_Attribute is new Root_Attributes with
      record
         T_Protocol: byte_arrays.Byte;
         Address:    Sock_Addr_Type;
         PeerID:     Peer_ID;
      end record;


end PPETP.Attributes.Ack_Target;
