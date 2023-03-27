with Ada.Streams;		use Ada.Streams;

with Packets.Protocol.Command;		    use Packets.Protocol.Command;
with Packets.Protocol.Command.Structure;    use Packets.Protocol.Command.Structure;
with Packets.Protocol.Utilities;            use Packets.Protocol.Utilities;
with Parsing_Buffers;                       use Parsing_Buffers;
with Network_Utilities;
with Auth.Profiles;
with Auth.Credentials;                      use Auth.Credentials;

with PPETP;					use PPETP;

with PPETP.Attributes;				use PPETP.Attributes;
with PPETP.Attributes.Ack_Target;		use PPETP.Attributes.Ack_Target;
with PPETP.Attributes.New_Peer;			use PPETP.Attributes.New_Peer;
with PPETP.Attributes.Old_Peer;			use PPETP.Attributes.Old_Peer;
with PPETP.Attributes.Peer_Credential;		use PPETP.Attributes.Peer_Credential;
with PPETP.Attributes.Puncturing;		use PPETP.Attributes.Puncturing;
with PPETP.Attributes.Nat_Parameter;		use PPETP.Attributes.Nat_Parameter;
with PPETP.Attributes.Routing_Probability;	use PPETP.Attributes.Routing_Probability;

with Ada.Exceptions;				use Ada.Exceptions;

with Ada.Unchecked_Conversion;
with Ada.Text_IO;use Ada.Text_IO;

with Interfaces;	use Interfaces;

package body Packets.Protocol.Command.Parsing is


   procedure Extract_Uint32 is
     new Extract (Bit_Field_32);

   procedure Extract_Uint16 is
     new Extract (Bit_Field_16);

   procedure Extract_Uint8 is
     new Extract (Bit_Field_8);

   procedure Get is
     new Get_Remaining (Target => byte_array_Pt);

   function Bit_Field_To_Peer_ID is
     new Ada.Unchecked_Conversion (Source => Bit_Field_32,
                                   Target => PPETP.Peer_ID);

   ---------------
   -- Is_Routed --
   ---------------
   function Is_Routed(Packet: Network_Packet) return Boolean is
   begin
      return Packet.Buffer(3) /= 0;
   end Is_Routed;
   pragma Inline (Is_Routed);

   --------------------------
   -- Parse_Command_Header --
   --------------------------

   procedure Parse_Command_Header
     (Received     : in out Parsing_Buffer;
      Header       : in out PPETP_Command_Header) is


      Bt : Unsigned_8;

   begin


      Extract_Uint8(Received, Bt);

      Header.Version := PPETP.Version_Type( (Bt and 16#C0#) / 2**6);
      Header.Command := ((Bt and 16#20#) / 2**5) = 1 ;
      Header.Padding := ((Bt and 16#10#) / 2**4) = 1 ;
      Header.Request := Request_Type'Val(Bt and 16#0F#);

      Extract_Uint8(Received, Bt);
      Header.Flags := Request_Flags(Bt);

      Extract_Uint8(Received, Bt);
      Header.RH_Length := RH_Length_Type(Bt);

      Extract_Uint8(Received, Bt);
      Header.PPETP_Magic := PPETP_Magic(Bt);


      declare
         Bt1, Bt2, Bt3, Bt4 : Unsigned_8;
      begin
         Extract_Uint8(Received, Bt1);
         Extract_Uint8(Received, Bt2);
         Extract_Uint8(Received, Bt3);
         Extract_Uint8(Received, Bt4);

         Header.Sequence_Num := COmmand_Sequence_Number(Natural(Bt4) + Natural(Bt3) * 2**8 +
                                                          Natural(Bt2) * 2**16 +
                                                          Natural(Bt1) * 2**24);
      end;

      Extract_Uint8(Received, Bt);
      Header.Sub_Seq_Num := Sub_Sequence_Number(Bt);


--        Put_Line("   Version: " & Header.Version'img);
--        Put_Line("   Command: " & Header.Command'img);
--        Put_Line("   Padding: " & Header.Padding'img);
--        Put_Line("   Request: " & Header.Request'img);
--        Put_Line("   Flags: " & Header.Flags'img);
--        Put_Line("   RH Length: " & Header.RH_Length'img);
--        Put_Line("   Magic: " & Header.PPETP_Magic'img);
--        Put_Line("   Sequence: " & Header.Sequence_Num'img);
--        Put_Line("   Sub_Seq: " & Header.Sub_Seq_Num'img);

      if ((not Header.Request'Valid) or
            (Header.Version /= PPETP.Protocol_Version)) then
         raise Invalid_Packet;
      end if;

      if (not Header.Command) then
         raise Invalid_Packet_Type with "Data packet to Command parser";
      end if;

   end Parse_Command_Header;


   --------------------
   -- Get_Ack_Number --
   --------------------

   procedure Get_Ack_Number (Source  :    in out Parsing_Buffer;
                             Seq_Num :       out PPETP.Command_Sequence_Number;
                             Sub_Seq_Num:    out PPETP.Sub_Sequence_Number) is
      Tmp : Bit_Field_32;
      Bt : Unsigned_8;

   begin



      -- Extract Sequence Number
      Extract_Uint8(Source, Bt);
      Tmp := Bit_Field_32(Bt) * 2**24;
      Extract_Uint8(Source, Bt);
      Tmp := Tmp + Bit_Field_32(Bt) * 2**16;
      Extract_Uint8(Source, Bt);
      Tmp := Tmp + Bit_Field_32(Bt) * 2**8;
      Extract_Uint8(Source, Bt);
      Tmp := Bit_Field_32(Bt);

      -- Extract SSN
      Extract_Uint8(Source, Bt);

      Sub_Seq_Num := PPETP.Sub_Sequence_Number(Bt);
      Seq_Num := PPETP.Command_Sequence_Number(Tmp);

   end Get_Ack_Number;






   ---------------------
   -- Extract_Peer_ID --
   ---------------------
   procedure Extract_Peer_ID(Source  : in out Parsing_Buffer;
                             ID :         out PPETP.Peer_ID) is
      Tmp : Bit_Field_32;
      Bt : Unsigned_8;
   begin
      Extract_Uint8(Source, Bt);
      Tmp := Bit_Field_32(Bt) * 2**24;
      Extract_Uint8(Source, Bt);
      Tmp := Tmp + Bit_Field_32(Bt) * 2**16;
      Extract_Uint8(Source, Bt);
      Tmp := Tmp + Bit_Field_32(Bt) * 2**8;
      Extract_Uint8(Source, Bt);
      Tmp := Tmp + Bit_Field_32(Bt);

      ID := PPETP.Peer_ID(Tmp);
   end Extract_Peer_ID;




   --------------------------
   -- Parse_Command_Packet --
   --------------------------

   function Parse_Command_Packet (Source : Network_Packet;
                                  PeerID : PPETP.Peer_ID)
                                  return Control_Packet is

      Header       : PPETP_Command_Header;
      --Sequence_Num : PPETP.Sequence_Number;
      Received     : Parsing_Buffer := Make_Parsing_Buffer (Source.Buffer);

      function Internal_Parse (Source : Byte_Array;
                               Peer   : Sock_Addr_Type)
                               return Control_Packet is

         Target_PeerID : PPETP.Peer_ID;
         Ack_To : Sock_Addr_Type := No_Sock_Addr;

      begin

       --  Put_Line("Pkt size: " & Received.Last'img);

--         Put_Line("Parse: 1");
         Parse_Command_Header (Received, Header);
--         Put_Line("Parse: 2");


         -- ********************************************************************
         --
         --	* if the RH_Length field id not zero is a Routed Packet
         --
         --      - Check the Target_ID to see if the packet is direct to this
         --        peer; if not copy the entire packet and encapsulate it in
         --        a Forward packet
         --
         --      - If it is directed to this peer extract the ACK_To attribute
         --	    and then elaborate normally the packet because the Source
         --	    signature is been removed in the Input Task
         --
         -- ********************************************************************

         if   (Header.RH_Length /= 0) then  -- RH_Length /= 0

--            Put_Line("RH_Length /= 0");



            -- Check the Target PeerID
            Extract_Peer_ID(Received, Target_PeerID);


            -- this is a routing packet but direct to another peer
            if Target_PeerID /= PeerID then

               -- P.S. The Source signature is re-tied in the Input Task
               return Control_Packet'(ACK_Target     => Ack_To,
                                      Sequence_Num   => Header.Sequence_Num,
                                      Sub_Seq_Num    => Header.Sub_Seq_Num,
                                      Address        => Peer,
                                      Command        => Forward,

                                      SourceID       => Target_PeerID,
                                      Data           => new Byte_Array'(Source)) ;
            end if;

            -- Extract Source Address
            declare
               Ack_To_Attribute: Access_Attribute_Class := null;

            begin

               Put_Line("This beautiful packet is for me!");

               Get_Next_Attribute(Buffer => Received,
                                  Result => Ack_To_Attribute);

               Ack_To := Get_Address(ACK_TARGET_Attribute(Ack_To_Attribute.all));
               Put_Line("i should ack to: " & image(Ack_To));
            end;

         end if;



         -- Remove Padding
         if Header.Padding then
            declare
               size : Byte;
            begin
               size := Data(Buffer => Received,
                            Pos    => Last(Received));
     --          Put_Line("Parser: Padding size: " & size'img);
               Remove_Last(Buffer   => Received,
                           How_Many => Byte_Array_Offset(size));
            end;


         end if;



         case Header.Request is


         when Hello =>

            declare
               Peer_Cred : Access_Attribute_Class := null;
            begin


               -- Read the PEER_CREDENTIALS Attribute if any
               if Natural(Remaining(Received)) > 0 then

                  Get_Next_Attribute(Buffer => Received,
                                     Result => Peer_Cred);
               end if;


               Die_If_Not_Empty (Received);

               return Control_Packet'(ACK_Target        => Ack_To,
                                      Sequence_Num      => Header.Sequence_Num,
                                      Sub_Seq_Num       => Header.Sub_Seq_Num,
                                      Address           => Peer,
                                      Command           => Hello,
                                      H_Peer_Credential => Peer_Cred);
            end;

         when Set_Default =>

            declare

               Details : Byte_Array_Pt;
               Ch: PPETP.Channel_ID := PPETP.Channel_ID(Bit_Field_8(Header.Flags) and 2#0000_1111#);

               use type Bit_Field_8;
            begin


               Get(Buffer => Received,
                   Data   => Details);

               Die_If_Not_Empty (Received);


               return Control_Packet'(ACK_Target     => Ack_To,
                                      Sequence_Num   => Header.Sequence_Num,
                                      Sub_Seq_Num    => Header.Sub_Seq_Num,
                                      Address        => Peer,
                                      Command        => Set_Default,
                                      Chann_Def      => PPETP.Channel_ID(Bit_Field_8(Header.Flags) and 2#0000_1111#),
                                      Default        => Details);
            end;

         when Acknowledge  =>


            declare

               function Flags_To_Reason is
                 new Ada.Unchecked_Conversion (Source => Request_Flags,
                                               Target => ACK_Reason_Type);

               Sequence_Num_ACKed     : PPETP.Command_Sequence_Number;
               Sub_Sequence_Num_ACKed : PPETP.Sub_Sequence_Number;
            begin
               Get_Ack_Number (Received, Sequence_Num_ACKed, Sub_Sequence_Num_ACKed);
               Die_If_Not_Empty (Received);

               return Control_Packet'(ACK_Target       => Ack_To,
                                      Sequence_Num     => Header.Sequence_Num,
                                      Sub_Seq_Num      => Header.Sub_Seq_Num,
                                      Address          => Peer,
                                      Command          => Acknowledge,
                                      ACKed_Number     => Sequence_Num_ACKed,
                                      ACKed_Sub_Number => Sub_Sequence_Num_ACKed,
                                      ACK_Reason       => Flags_To_Reason(Header.Flags));
            end;

         when Data_Control  =>

            declare

               function Byte_To_Sub_Command is
                 new Ada.Unchecked_Conversion (Source => Bit_Field_8,
                                               Target => Data_Control_Sub_Command);

               Field_8, Param1, Param2, Param3 : Bit_Field_8;
               SC : Data_Control_Sub_Command;

               D_New_Peer        : Access_Attribute_Class; -- Start, Redirect
               D_Peer_Credential : Access_Attribute_Class; -- [Start, Redirect]
               D_Puncturing      : Access_Attribute_Class; -- [Start, Redirect]
               D_Routing_Prob    : Access_Attribute_Class; -- [Start, Redirect]
               D_Old_Peer        : Access_Attribute_Class; -- Stop, Redirect
               D_NAT_Param       : Access_Attribute_Class; -- [Punch]
            begin

               Extract_Uint8(Received, Field_8);
               SC := Byte_To_Sub_Command(Field_8);

--               Put_Line("SC " & SC'img);


               Extract_Uint8(Received, Param1);
               Extract_Uint8(Received, Param2);
               Extract_Uint8(Received, Param3);

--             Put_Line("Ch: " & Param1'img);

               while Natural(Remaining(Received)) > 0 loop

                  declare
                     Tmp_Attr : Access_Attribute_Class;
                  begin

--                     Put_Line("Parse 3");
                     Get_Next_Attribute(Buffer => Received,
                                        Result => Tmp_Attr);
--                     Put_Line("Parse 4");

                     case Get_Type(Tmp_Attr) is
                        when NEW_PEER_Attribute_Index =>
                           D_New_Peer := Tmp_Attr;

                        when OLD_PEER_Attribute_Index =>
                           D_Old_Peer := Tmp_Attr;

                        when PEER_CREDENTIAL_Attribute_Index =>
                           D_Peer_Credential := Tmp_Attr;

                        when PUNCTURING_Attribute_Index =>
                           D_Puncturing := Tmp_Attr;

                        when NAT_PARAMETER_Attribute_Index =>
                           D_NAT_Param := Tmp_Attr;

                        when ROUTING_PROBABILITY_Attribute_Index =>
                           D_Routing_Prob := Tmp_Attr;

                        when others =>
                           Raise_Exception(E       => Program_Error'Identity,
                                           Message => "Unknown Attribute");
                     end case;

--                     Put_Line("Parse 5");
                  end;

--                  Put_Line("Remaining: " & Natural(Remaining(Received))'img);
               end loop;
--               Put_Line("Parse 6");


               return Control_Packet'(ACK_Target     => Ack_To,
                                      Sequence_Num => Header.Sequence_Num,
                                      Sub_Seq_Num  => Header.Sub_Seq_Num,
                                      Address      => Peer,
                                      Command      => Data_Control,
                                      SC           => SC,
                                      Param_1      => Byte(Param1),
                                      Param_2      => Byte(Param2),
                                      Param_3      => Byte(Param3),
                                      D_New_Peer        => D_New_Peer,
                                      D_Peer_Credential => D_Peer_Credential,
                                      D_Puncturing      => D_Puncturing,
                                      D_Routing_Prob    => D_Routing_Prob,
                                      D_Old_Peer        => D_Old_Peer,
                                      D_NAT_Param       => D_NAT_Param
                                     );
            end;

         when others =>
            Put_Line("No valid Packet");
            raise Program_Error;

         end case;

      end Internal_Parse;


      Result    : Control_Packet :=
        Internal_Parse (Source.Buffer, Source.Peer);
   begin

--      Put_Line("Parser: command: " & Result.Command'img);
--      Put_Line("Parser: Request: " & Header.Request'img);
      if Result.Command /= FORWARD then
         null;
         pragma Assert (Result.Command = Header.Request);
      end if;

      return Result;
   end Parse_Command_Packet;


end Packets.Protocol.Command.Parsing;
