with Ada.Streams;			use Ada.Streams;
with Ada.Unchecked_Conversion;
with PPETP.Attributes;			use PPETP.Attributes;
with Network_Utilities;			use Network_Utilities;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;
with byte_arrays;			use byte_arrays;
with Interfaces;

with Ada.Text_IO;		use Ada.Text_IO;

package body PPETP.Attributes.Ack_Target is

   ------------------
   -- Parsing_Data --
   ------------------
   function Parsing_Data(Data: byte_Array) return Access_Attribute_Class is

      type Peer_Id_Buffer is new Byte_Array(1..4);
      function Byte_Arr_To_PeerID is
        new Ada.Unchecked_Conversion (Source => Peer_Id_Buffer,
                                      Target => Peer_ID);

      Version  : byte_arrays.Byte;

      Protocol : byte_arrays.Byte;
      Port     : Bit_Field_16;
   begin
      --Put_Line(Data'First'img & " " & Data'Last'img);
      Version  := byte_arrays.Byte( (byte_arrays.Byte(Data(1)) and 2#1111_0000#) / 2**4);
      Protocol := Data(2);
      Port     := Bit_Field_16((Natural(Data(3)) * 2**8 + Natural(Data(4))) mod 2**16);

      if Version = 4 then
         declare
            IP : Inet_Addr_V4_Buffer;
            Id : Peer_ID;
         begin

            for i in  1 .. 4 loop
               IP(i) := Interfaces.Unsigned_8(Data( Stream_Element_Offset(4+i)));
            end loop;

            Id := Byte_Arr_To_PeerID(Peer_Id_Buffer(Data(9..12)));

            declare

               Addr : Sock_Addr_Type := (Family =>Family_Inet,
                                         Addr   => Inet_Addr(IP),
                                         Port   => Port_Type(Port));
            begin

               return Access_Attribute_Class'( new ACK_TARGET_Attribute'(Attr_Type  => ACK_TARGET_Attribute_Index,
                                                                         T_Protocol => Protocol,
                                                                         Address    => Addr,
                                                                         PeerID     => Id));
            end;
         end;
      else
         declare
            IP : Inet_Addr_V6_Buffer;
            Id : Peer_ID;
         begin
            for i in 1 .. 16 loop
               IP(i) := Interfaces.Unsigned_8(Data(Stream_Element_Offset(4+i)));
            end loop;

            Id := Byte_Arr_To_PeerID(Peer_Id_Buffer(Data(21..24)));

            declare

               Addr : Sock_Addr_Type := (Family =>Family_Inet6,
                                         Addr   => Inet_Addr(IP),
                                         Port   => Port_Type(Port));
            begin

               return Access_Attribute_Class'( new ACK_TARGET_Attribute'(Attr_Type  => ACK_TARGET_Attribute_Index,
                                                                         T_Protocol => Protocol,
                                                                         Address    => Addr,
                                                                         PeerID     => Id));
            end;
         end;
      end if;

   end Parsing_Data;

   -------------------
   -- Building_Data --
   -------------------
   function Building_Data(Object: ACK_TARGET_Attribute) return byte_array is
      type Peer_Id_Buffer is new Byte_Array(1..4);
      function PeerID_To_Byte_Arr is
        new Ada.Unchecked_Conversion (Source => Peer_ID,
                                      Target => Peer_Id_Buffer);
   begin



      if Object.Address.Family = Family_Inet then -- for IPv4
         declare
            IP : Inet_Addr_V4_Buffer;
            Payload : Byte_Array(1..12);
         begin
           -- Put_Line("Attr: Qua 1!");
            Payload(1) := 4 * 2**4;
            Payload(2) := Object.T_Protocol;

            Payload(3) := byte_arrays.Byte(Object.Address.Port / 2**8);
            Payload(4) := byte_arrays.Byte(Object.Address.Port mod 2**8);

            IP := To_Array(Addr => Object.Address.Addr);
            for i in  1 .. 4 loop
               Payload( Stream_Element_Offset(4+i)) := byte_arrays.Byte(IP(i));
            end loop;

            Payload(9..12) := Byte_Array(PeerID_To_Byte_Arr(Object.PeerID));

            return Payload;
         end;

      else -- for IPv6
         declare
            IP : Inet_Addr_V6_Buffer;
            Payload : Byte_Array(1..24);
         begin
            Payload(1) := 6 * 2**4;
            Payload(2) := Object.T_Protocol;

            Payload(3) := byte_arrays.Byte(Object.Address.Port / 2**8);
            Payload(4) := byte_arrays.Byte(Object.Address.Port mod 2**8);

            IP := To_Array(Addr => Object.Address.Addr);
            for i in  1 .. 16 loop
               Payload( Stream_Element_Offset(4+i)) := byte_arrays.Byte(IP(i));
            end loop;

            Payload(21..24) := Byte_Array(PeerID_To_Byte_Arr(Object.PeerID));

            return Payload;
         end;
      end if;

   end  Building_Data;


   --------------------
   -- Set_Attributes --
   --------------------
   procedure Set_Attribute(Object :    in out ACK_TARGET_Attribute;
                           Address:    in     Sock_Addr_Type;
                           T_Protocol: in     byte_arrays.Byte;
                           PeerID:     in     Peer_ID) is
   begin
      Object.Attr_Type  := ACK_TARGET_Attribute_Index;
      Object.T_Protocol := T_Protocol;
      Object.Address	:= Address;
      Object.PeerID	:= PeerID;
   end Set_Attribute;


   --------------------
   -- Get_Attributes --
   --------------------
   procedure Get_Attribute(Object :    in out ACK_TARGET_Attribute;
                           Attr_Type:     out Attributes_Index_Type;
                           Address:       out Sock_Addr_Type;
                           T_Protocol:    out byte_arrays.Byte;
                           PeerID:        out Peer_ID) is
   begin
      T_Protocol := Object.T_Protocol;
      Address    := Object.Address;
      Attr_Type  := Object.Attr_Type;
      PeerID	 := Object.PeerID;
   end Get_Attribute;

   -----------------
   -- Get_Address --
   -----------------
   function Get_Address(Object : ACK_TARGET_Attribute) return Sock_Addr_Type is
   begin
      return Object.Address;
   end Get_Address;

   ----------------
   -- Get_PeerID --
   ----------------
   function Get_PeerID(Object : ACK_TARGET_Attribute) return Peer_ID is
   begin
      return Object.PeerID;
   end Get_PeerID;


   -----------
   -- Image --
   -----------
   procedure Image(Object : ACK_TARGET_Attribute) is
   begin
      Put_Line("Protocol: " & Object.T_Protocol'img);
      Put_Line("Address: " & Image(Object.Address));
      Put_Line("PeerID: " & Object.PeerID'img);
   end Image;


begin

   PPETP.Attributes.Register(Index    => ACK_TARGET_Attribute_Index,
                             Callback => Parsing_Data'Access);

end PPETP.Attributes.Ack_Target;
