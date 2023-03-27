with Packets.Protocol.Utilities;         use Packets.Protocol.Utilities;
with Network;                            use Network;
with Interfaces;                         use Interfaces;
with Ada.Streams;                        use Ada.Streams;
with Common_Types;                       use Common_Types;
with Parsing_Buffers;                    use Parsing_Buffers;
with Packets.Protocol.Data.Structure;    use Packets.Protocol.Data.Structure;

with System;
with Interfaces;			use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Text_IO;	use ada.Text_IO;

package body Packets.Protocol.Data.Parsing is


    procedure Extract_Uint8 is
     new Extract (Bit_Field_8);

   -----------------------
   -- Parse_Data_Header --
   -----------------------
   -- Extract header information from a network packet

   procedure Parse_Data_Header
     (Received     : in out Parsing_Buffer;
      Header       : in out PPETP_Data_Header) is


      Bt : Unsigned_8;

      use type PPETP.Version_Type;


   begin

      Extract_Uint8(Received, Bt);

      Header.Version := PPETP.Version_Type( (Bt and 16#C0#) / 2**6);
      Header.Command := ((Bt and 16#20#) / 2**5) = 1 ;
      Header.Padding := ((Bt and 16#10#) / 2**4) = 1 ;
      Header.Inline  := ((Bt and 16#08#) / 2**3) = 1 ;
      Header.Prof_Flags := Profile_Flags((Bt and 16#07#));

      Extract_Uint8(Received, Bt);
      Header.Channel  := PPETP.Channel_ID((Bt and 16#F0#) / 2**4);

      Extract_Uint8(Received, Bt); -- unused byte
      Header.Reserved := 0;

      Extract_Uint8(Received, Bt);
      Header.Ppetp_Magic := PPETP.PPETP_Magic(Bt);


      declare
         Bt1, Bt2, Bt3, Bt4, Bt_Tmp : Unsigned_8;
         U16: Unsigned_16;
         U32: Unsigned_32;
      begin
         Extract_Uint8(Received, Bt1);
         Extract_Uint8(Received, Bt2);
         Extract_Uint8(Received, Bt3);
         Extract_Uint8(Received, Bt4);


--         Put(Bt1'img & Bt2'img & Bt3'img & Bt4'img);


         -- Stream_ID
         Bt_Tmp := (Bt2 and 16#F0#) / 2**4; -- Lsb of the StreamID

         U16 := Unsigned_16(Bt1) * 2**4 + Unsigned_16(Bt_Tmp);

         Header.Stream_ID := Stream_ID(U16);

         -- Sequence Number
         Bt_Tmp := (Bt2 and 16#0F#);

         U32 := Unsigned_32(Bt_Tmp) * 2**16 + Unsigned_32(Bt3) * 2**8 + Unsigned_32(Bt4);

--         Put_line(" --> " & U32'img);
         Header.Sequence_Num := Data_Sequence_Number(U32);
--         Put_Line(Header.Sequence_Num'img);

      end;


      if (Header.Version /= PPETP.Protocol_Version) then
         raise Invalid_Packet;
      end if;

      if (Header.Command) then
         raise Invalid_Packet_Type with "Command packet to Data parser";
      end if;


   end Parse_Data_Header;

   -----------------------
   -- Parse_Data_Packet --
   -----------------------

   function Parse_Data_Packet (Source : Network_Packet)
                               return Data_Packet is

      procedure Get is
        new Get_Remaining (Target => byte_array_Pt);

      Header       : PPETP_Data_Header;
      Received     : Parsing_Buffer := Make_Parsing_Buffer (Source.Buffer);
   begin


      Parse_Data_Header (Received, Header);

      if (Header.Padding) then
         declare
            Padding_Size : Stream_Element_Offset :=
              Stream_Element_Offset (Received.Data (Received.Last));

         begin
           -- Put_Line("Parser: Padding size: " & Padding_Size'img);
            Remove_Last (Received, Padding_Size);
         end;
      end if;


      declare
         Payload : Raw_Data;
      begin



         Get(Received, Payload.Data);

--         Put_Line("Parser Data length: " & Payload.Data'length'img);

         Payload.Inline := Header.Inline;
         Payload.Flags  := Header.Prof_Flags;

         --************* TODO ********************
	 --*   Read the forwarder Signature here *
      	 --***************************************



         Die_If_Not_Empty (Received);

         -- Finally, compose the result.
         return Data_Packet'(Sequence_Num => Header.Sequence_Num,
                             Address	  => Peer(Source),
                             StreamID     => Header.Stream_ID,
                             Channel      => Header.Channel,
                             Payload      => Payload);
      end;
   end Parse_Data_Packet;

end Packets.Protocol.Data.Parsing;
