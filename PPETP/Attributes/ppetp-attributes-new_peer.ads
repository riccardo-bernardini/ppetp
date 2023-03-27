with Network;		use Network;
with byte_arrays;	use byte_arrays;
with PPETP;		use PPETP;

package PPETP.Attributes.New_Peer is

   NEW_PEER_Attribute_Index : constant Attributes_Index_Type := 0;


   type NEW_PEER_Attribute is
     new Root_Attributes with private;

   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class;
   function Building_Data(Object: NEW_PEER_Attribute) return byte_array;

   procedure Set_Attribute(Object :    in out NEW_PEER_Attribute;
                           Address:    in     Sock_Addr_Type;
                           T_Protocol: in     byte_arrays.Byte;
                           PeerID:     in     Peer_ID);

   procedure Get_Attribute(Object :    in out NEW_PEER_Attribute;
                           Attr_Type:     out Attributes_Index_Type;
                           Address:       out Sock_Addr_Type;
                           T_Protocol:    out byte_arrays.Byte;
                           PeerID:        out Peer_ID);

   function Get_Address(Object : NEW_PEER_Attribute) return Sock_Addr_Type;

   function Get_PeerID(Object : NEW_PEER_Attribute) return Peer_ID;

   procedure Image(Object : NEW_PEER_Attribute);

private

   type NEW_PEER_Attribute is new Root_Attributes with
      record
         T_Protocol: byte_arrays.Byte;
         Address:    Sock_Addr_Type;
         PeerID:     Peer_ID;
      end record;


end PPETP.Attributes.New_Peer;
