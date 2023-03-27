with PPETP;				use PPETP;
with PPETP.Attributes.Ack_Target;		use PPETP.Attributes.Ack_Target;
with Packets.Protocol.Command.Structure;    use Packets.Protocol.Command.Structure;
with Packets.Protocol.Utilities;            use Packets.Protocol.Utilities;
with Byte_Arrays;                           use Byte_Arrays;
with Network_Utilities;
with Auth.Profiles;

with Interfaces;		use Interfaces;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Text_IO; use Ada.Text_IO;

package body packets.protocol.command.building is

   function To_PPETP_Command_Buffer(Source : PPETP_Command_Header) return PPETP_Command_Header_Buffer is
      Tmp : PPETP_Command_Header_Buffer;
      Bt : Unsigned_8;
   begin

--        Put_Line("Version: " & Source.Version'img);
--        Put_Line("Command: " & Source.Command'img);
--        Put_Line("Padding: " & Source.Padding'img);
--        Put_Line("Request: " & Source.Request'img);
--        Put_Line("Flags: " & Source.Flags'img);
--        Put_Line("RH Length: " & Source.RH_Length'img);
--        Put_Line("Magic: " & Source.PPETP_Magic'img);
--        Put_Line("Sequence: " & Source.Sequence_Num'img);
--	  Put_Line("Sub_Seq: " & Source.Sub_Seq_Num'img);


      Bt := Unsigned_8( Source.Version) * 2**6;

      -- is a command
      Bt := Bt + 2**5;

      -- padding
      if Source.Padding then
         Bt := Bt + 2**4;
      end if;

      -- Request
      Bt := Bt + Request_Type'Pos(Source.Request);

      Tmp(1) := Byte(Bt);
      Tmp(2) := Byte(Source.Flags);
      Tmp(3) := Byte(Source.RH_Length);
      Tmp(4) := Byte(Source.PPETP_Magic);

      Tmp(5) := Byte( (Bit_Field_32(Source.Sequence_Num) and 16#FF_00_00_00#) / 2**24);
      Tmp(6) := Byte( (Bit_Field_32(Source.Sequence_Num) and 16#00_FF_00_00#) / 2**16);
      Tmp(7) := Byte( (Bit_Field_32(Source.Sequence_Num) and 16#00_00_FF_00#) / 2**8);
      Tmp(8) := Byte( (Bit_Field_32(Source.Sequence_Num) and 16#00_00_00_FF#));

      Tmp(9) := Byte(Source.Sub_Seq_Num);

      return Tmp;
   end  To_PPETP_Command_Buffer;


   subtype Uint32_Buf is Byte_Array (1 .. 4);

   function bit_field_32_To_Buf is
     new Ada.Unchecked_Conversion (Source => bit_field_32,
                                   Target => Uint32_Buf);

   -----------------
   -- Get_Padding --
   -----------------
   function Get_Padding (Length : Natural)
                           return Byte_Array is
         Padding_Length : Byte_Array_Offset;
         Padding_Buffer : Byte_Array (1..4);
   begin


      if (Length mod 4 = 0) then
         return Padding_Buffer(1..0); -- Empty
      else
         Padding_Length := Byte_Array_Offset(4 - Length mod 4);
         Padding_Buffer(Padding_Length) := Byte(Padding_Length);

         return Padding_Buffer(1..Padding_Length);
      end if;
   end Get_Padding;


   ----------------------
   -- Make_ACK_Payload --
   ----------------------

   function Make_ACK_Payload (Source : Control_Packet)
                              return byte_array is
      tmp : Byte_Array(1..5);

   begin
      pragma Assert (Source.Command = Acknowledge);


      tmp(1) := Byte( (Bit_Field_32(Source.ACKed_Number) and 16#FF_00_00_00#) / 2**24 );
      tmp(2) := Byte( (Bit_Field_32(Source.ACKed_Number) and 16#00_FF_00_00#) / 2**16 );
      tmp(3) := Byte( (Bit_Field_32(Source.ACKed_Number) and 16#00_00_FF_00#) / 2**8 );
      tmp(4) := Byte( (Bit_Field_32(Source.ACKed_Number) and 16#00_00_00_FF#) );
      tmp(5) := Byte(Source.ACKed_Sub_Number);
      return tmp;
   end Make_ACK_Payload;


   ------------------------
   -- Make_HELLO_Payload --
   ------------------------
   function Make_HELLO_Payload (Source : Control_Packet)
                                return byte_array is
   begin
      pragma Assert (Source.Command = Hello);

      return Get_Attribute_Packet(Source.H_Peer_Credential);

   end Make_HELLO_Payload;


   ------------------------------
   -- Make_SET_DEFAULT_Payload --
   ------------------------------
   function Make_SET_DEFAULT_Payload (Source  : Control_Packet)
                                      return byte_array is
   begin
      return Source.Default.all;
   end Make_SET_DEFAULT_Payload;


   -------------------------------
   -- Make_DATA_CONTROL_Payload --
   -------------------------------
   function Make_DATA_CONTROL_Payload (Source  : Control_Packet)
                                       return byte_array is
      function Sub_Command_To_Byte is
        new Ada.Unchecked_Conversion (Source => Data_Control_Sub_Command,
                                      Target => Byte);

      function Add_Attribute(Attr : Access_Attribute_Class)
                             return byte_Array is
         Data : Byte_Array(1..2) := (others =>0);
      begin
         if Attr = null then
            return Data(1..0); --empty
         else
            return Get_Attribute_Packet(Attr);
         end if;
      end  Add_Attribute;

      Head : byte_Array(1..4);
      Length : Natural := 0;
   begin






      Head(1..4) := Sub_Command_To_Byte(Source.SC) & Source.Param_1 & Source.Param_2 & Source.Param_3;

      return Head & Add_Attribute(Source.D_New_Peer) & Add_Attribute(Source.D_Old_Peer) &
      Add_Attribute(Source.D_Peer_Credential) & Add_Attribute(Source.D_Puncturing) &
      Add_Attribute(Source.D_Routing_Prob) & Add_Attribute(Source.D_Old_Peer) &
      Add_Attribute(Source.D_NAT_Param);



   end Make_DATA_CONTROL_Payload;


   ---------------------
   -- Peer_ID_To_Byte --
   ---------------------
   function Peer_ID_To_Byte(PeerID: Peer_ID) return Byte_Array is
      Tmp : Byte_Array(1..4);

   begin
      Tmp(1) := Byte( (Bit_Field_32(PeerID) and 16#FF_00_00_00#) /2**24  ) ;
      Tmp(2) := Byte( (Bit_Field_32(PeerID) and 16#00_FF_00_00#) /2**16  ) ;
      Tmp(3) := Byte( (Bit_Field_32(PeerID) and 16#00_00_FF_00#) /2**8   ) ;
      Tmp(4) := Byte( (Bit_Field_32(PeerID) and 16#00_00_00_FF#)        ) ;

      return Tmp;
   end Peer_ID_To_Byte;




   -------------------------
   -- Make_Control_Packet --
   -------------------------

   function Make_Control_Packet (Source     : Control_Packet;
                                 Routed     : Boolean;
                                 Target_ID  : Peer_ID;
                                 Ack_Target : ACK_TARGET_Attribute)
                                 return Network_Packet is


      function Reason_To_Flags is
        new Ada.Unchecked_Conversion (Source => ACK_Reason_Type,
                                      Target => Request_Flags);


      function Get_Routing_Info (Routed     : Boolean;
                                 Target_ID  : Peer_ID;
                                 Ack_Target : ACK_TARGET_Attribute) return Byte_Array is
         No_Info : Byte_Array(1..0);

      begin

         --Put_Line("Get_Routing_Info");
         declare
            Target : Access_Attribute_Class := new ACK_TARGET_Attribute'(Ack_Target);
         begin

            if not Routed then
               return No_Info;
            end if;
           -- Put_Line("Get_Routing_Info 1");
            return Peer_ID_To_Byte(Target_ID) & Get_Attribute_Packet(Target);
         end;
      end Get_Routing_Info;

--        function Glue (Basic_Header : PPETP_Command_Header;
--                       Routed       : Boolean;
--                       Target_ID    : Peer_ID;
--                       Ack_Target   : ACK_TARGET_Attribute;
--                       Payload      : byte_array)
--                       return Network_Packet is
--        begin
--           return New_Packet(To_PPETP_Command_Buffer (Basic_Header) &
--                             Get_Routing_Info(Routed, Target_ID, Ack_Target) & Payload,
--                             Source.Address);
--        end Glue;

      Basic_Header    : PPETP_Command_Header;
      Routing_Target_Header : Byte_Array := Get_Routing_Info(Routed, Target_ID, Ack_Target);
   begin

     -- Put_Line("Building 1");
      Basic_Header := (Version      => PPETP.Protocol_Version,
                       Command      => True,
                       Padding	    => <>,
                       Request      => Source.Command,
                       Flags	    => <>,
                       RH_Length    => <>,
                       PPETP_Magic  => PPETP.PPETP_Magic_Default,
                       Sequence_Num => Source.Sequence_Num,
                       Sub_Seq_Num  => Source.Sub_Seq_Num);

      --   Put_Line("Building 2: " & Source.Command'img);
--        if Routed then
--           Put_Line("Building: Target: " & Target_ID'img);
--           Put_Line("Building: Command: " & Source.Command'img);
--        end if;

      if Routed then
         Basic_Header.RH_Length := Routing_Target_Header'Length;
--         Put_Line("************    Builder: RH_Length:   " & Basic_Header.RH_Length'img);
      end if;

      case Source.Command is


         when Acknowledge =>

            Basic_Header.RH_Length   := 0;     -- an ACK is never Routed
            Basic_Header.Flags	     := Reason_To_Flags(Source.ACK_Reason);


            declare
               Payload : byte_array := Make_Ack_Payload (Source);
               Padding : byte_array := Get_Padding (Payload'Length);
            begin

               Basic_Header.Padding   := Padding'Length /= 0;


               return New_Packet(To_PPETP_Command_Buffer (Basic_Header) &
                                 Payload & Padding,
                                 Source.Address);
            end;


         when Hello =>
            Basic_Header.Flags     := 0;

            Image(Ack_Target);


            if Source.H_Peer_Credential /= null then

               declare
                  Payload : byte_array := Make_HELLO_Payload (Source);
                  Padding : byte_array := Get_Padding (Payload'Length);
               begin

                  Basic_Header.Padding   := Padding'Length /= 0;


                  return New_Packet(To_PPETP_Command_Buffer (Basic_Header) &
                                    Routing_Target_Header & Payload & Padding,
                                    Source.Address);

--                  return Glue (Basic_Header, Routed, Target_ID, Ack_Target, Payload & Padding);
               end;
            else
               Basic_Header.Padding := False;

               -- It has not payload and padding

               return New_Packet(To_PPETP_Command_Buffer (Basic_Header) &
                                 Routing_Target_Header,
                                 Source.Address);
            end if;


         when Set_Default =>
            declare
               Payload : byte_array := Make_SET_DEFAULT_Payload (Source);
               Padding : byte_array := Get_Padding (Payload'Length);
            begin
               Basic_Header.Padding   := Padding'Length /= 0;
               Basic_Header.Flags     := Request_Flags(Source.Chann_Def);


--               return Glue (Basic_Header,  Routed, Target_ID, Ack_Target, Payload & Padding);
               return New_Packet(To_PPETP_Command_Buffer (Basic_Header) &
                                 Routing_Target_Header & Payload & Padding,
                                 Source.Address);
            end;

         when Data_Control =>
       --     Put_Line("Building 3");
            declare
               Payload : byte_array := Make_DATA_CONTROL_Payload (Source);
               Padding : byte_array := Get_Padding (Payload'Length);
            begin
               Basic_Header.Padding   := Padding'Length /= 0;

               Basic_Header.Flags     := 0;
               --               return Glue (Basic_Header,  Routed, Target_ID, Ack_Target, Payload & Padding);
               return New_Packet(To_PPETP_Command_Buffer (Basic_Header) &
                                 Routing_Target_Header & Payload & Padding,
                                 Source.Address);
            end;

         when Forward =>

            return New_Packet(Source.Data.all, Source.Address);


      end case;

   end Make_Control_Packet;
end packets.protocol.command.building;
