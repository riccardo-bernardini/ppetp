with PPETP.Attributes;			use PPETP.Attributes;


-- A record with all the kinds of attributes;

package PPETP.Attributes.Attributes_Records is

   type Attributes_Record is record
      Ack_Target      : Access_Attribute_Class := null;
      Nat_Parameters  : Access_Attribute_Class := null;
      New_Peer        : Access_Attribute_Class := null;
      Old_Peer        : Access_Attribute_Class := null;
      Peer_Credential : Access_Attribute_Class := null;
      Puncturing      : Access_Attribute_Class := null;
      Routing         : Access_Attribute_Class := null;
   end record;


end PPETP.Attributes.Attributes_Records;
