with Packets.Protocol.Data.Structure;    use Packets.Protocol.Data.Structure;
with Packets.Protocol.Utilities;         use Packets.Protocol.Utilities;
with Byte_Arrays;                        use Byte_Arrays;

with Interfaces;		use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Text_IO;	use Ada.Text_IO;

package body Packets.Protocol.Data.Building is
   -----------------
   -- Make_Packet --
   -----------------

   function Make_Data_Packet (Source  : Data_Packet)
                             return Network_Packet is

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

      subtype Header_Buffer is
        Byte_Array (1..Byte_Array_Offset(PPETP_Data_Header_Size));


      function Header_To_Bytes (Source : PPETP_Data_Header)
                                return Byte_Array is
         Tmp : PPETP_Data_Header_Buffer;
         Bt, Bt1 : Unsigned_8;
         U16: Unsigned_16;
      begin

         --version
         Bt := Unsigned_8( Source.Version) * 2**6;

         -- padding
         if Source.Padding then
            Bt := Bt + 2**4;
         end if;

         -- inline
         if Source.Inline then
            Bt := Bt + 2**3;
         end if;

         -- Profile Flags
         Bt := Bt + Unsigned_8(Source.Prof_Flags);

         Tmp(1) := Byte(Bt);
         Tmp(2) := Byte( Unsigned_8(Source.Channel) * 2**4);
         Tmp(3) := 0;
         Tmp(4) := Byte(Source.PPETP_Magic);

         -- The stream Id is a 12 bits number
         U16 := Unsigned_16(Source.Stream_ID);
         Tmp(5) := Byte( (Unsigned_16(Source.Stream_ID) and 16#0F_F0#) / 2**4);

         -- The Lsb of the Stream_ID is the Msb of a byte shared
         -- with the 4 Msb of the Sequence_Number
         Bt := Unsigned_8( (Unsigned_16(Source.Stream_ID) and 16#00_0F#) * 2**4);

         -- The Sequence Number is a 20 bits number
         Bt1 := Unsigned_8( (Unsigned_32(Source.Sequence_Num) and 16#00_0F_00_00#) / 2**16);

         Tmp(6) := Byte(Bt+Bt1);
         Tmp(7) := Byte((Unsigned_32(Source.Sequence_Num) and 16#00_00_FF_00#) / 2**8);
         Tmp(8) := Byte((Unsigned_32(Source.Sequence_Num) and 16#00_00_00_FF#) );

         --Put(Tmp(5)'img & Tmp(6)'img & Tmp(7)'img & Tmp(8)'img);
         --Put_Line(" --> " & Source.Sequence_Num'img);

         return Tmp;
      end Header_To_Bytes;



      function Channel_To_Flags is
        new Ada.Unchecked_Conversion (Source => PPETP_Channel_ID,
                                      Target => Bit_Field_4);


      Basic_Header  : PPETP_Data_Header;

      Payload       : Byte_Array_Pt;



      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Byte_Array,
                                        Name   => Byte_Array_Pt);
   begin

      Payload := Source.Payload.Data;

--      Put_Line("Builder Data Length " & Payload'length'img);

      declare
         Padding : Byte_Array := Get_Padding (Payload'Length);

      begin


         Basic_Header := (Version        => PPETP.Protocol_Version,
                          Command        => False,
                          Padding        => Padding'Length /= 0,
                          Inline         => Source.Payload.Inline,
                          Prof_Flags     => Source.Payload.Flags,
                          Channel	 => Source.Channel,
                          Reserved       => 0,
                          Ppetp_Magic    => PPETP.PPETP_Magic_Default,
                          Stream_ID      => Source.StreamID,
                          Sequence_Num   => Source.Sequence_Num);

         declare
            Result : Network_Packet :=
              New_Packet(Header_To_Bytes (Basic_Header) & Payload.all & Padding);

         begin
            Result.Set_Peer(Source.Address);
            Free (Payload);
            return Result;
         end;
      end;
   end Make_Data_Packet;

end Packets.Protocol.Data.Building;
