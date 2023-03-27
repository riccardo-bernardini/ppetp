with Timestamps;               use Timestamps;
with Packets.External.Network; use Packets.External.Network;
with Packets.Network_Crumbs;   use Packets.Network_Crumbs;
with Interfaces;               use Interfaces;

use Packets;
use Packets.External;
package body Merging is
   --
   -- =================================
   -- = Structure of a Network_Packet =
   -- =================================
   --
   -- A Network_Packet is a "shell" which contains several
   -- Network_Crumb's.  A Network_Packet is made of an header
   -- and the payload.
   --
   --   * The HEADER is 5 bytes long
   --      - byte 0..3  => Timestamp as an Unsigned_Integer32
   --      - byte 4     => N. of crumbs-1
   --
   --   * The PAYLOAD is a sequence of Network_Crumb's, each crumb
   --     preceded by a SUB_HEADER 2 bytes long
   --       - bit 0..14 => # bytes of the crumb
   --       - bit 15    => (0 -> analog crumb, 1 -> binary crumb)
   --       - bit 16    => Reserved (e.g. for future classes)
   --

   -- We need 5 bytes to store the timestamp and
   -- N. of subpackets
   Header_Size : constant Positive := 5;

   -- Every subpacket requires 2 more bytes to store
   -- packet length and type.
   Sub_Header_Size : constant Positive := 2;




   procedure Synthesize (Input   : in     Network_Crumb_Array;
                         Output  :    out Network_Packet;
                         Success :    out Boolean) is
      procedure Write_Header (Timestamp  : in     Timestamp_Type;
                              N_Packets  : in     Sub_Packets_Count;
                              Output :    out Network_Packet) is
      begin
         Write (Packet => Output,
                Value  => Integer(Timestamp),
                N_Bits => 32);

         Write (Packet => Output,
                Value  => N_Packets-1,
                N_Bits => 8);
      end Write_Header;

      procedure Write_Sub_Header (Packet     : in     Network_Crumb;
                                  Output :    out Network_Packet) is
         Len : Natural;
         Class_Field : Natural;
      begin
         case Packet.Class is
            when Analog =>
               Class_Field := 0;
            when Binary =>
               Class_Field := 1;
         end case;

         Write (Packet => Output,
                Value  => Packet.Payload'Length + Class_Field * 2**14,
                N_Bits => 16);
      end Write_Sub_Header;

      function Total_Packet_Length(Input : Network_Crumb_Array)
                                  return Natural is
         Total_Len : Natural;
      begin
         Total_Len := Input'Length * Sub_Header_Size + Header_Size;

        -- Space needed by the payload
        for I in Input'Range loop
           Total_Len := Total_Len + Payload_Size(Input(I));
        end loop;

        return Total_Len;
      end Total_Packet_Length;
   begin
      -- Now we can initialize the packet
      Resize(Output, Total_Packet_Length(Input));

      Write_Header(Timestamp => Input.Timestamp,
                   N_Packets => Input'Length,
                   Output    => Output);

      for I in Input'Range loop
         Write_Sub_Header (Sub_Packet => Input(I),
                           Output     => Output);

         Append(Output, Input(I).Class);
      end loop;

   end Synthesize;

   function Split(Input : Network_Packet) return Network_Crumb_Array is
      Timestamp : Network_Timestamp;
      N_Crumbs  : Integer;
   begin
      Rewind(Input);
      Read (Packet => Input,
            Value  => Timestamp,
            N_Bits => 32);

      Read (Packet => Input,
            Value  => N_Crumbs,
            N_Bits => 8);

      declare
         Result      : Network_Crumb_Array(1..N_Crumbs);
         Crumb_Size  : Positive;
         Buffer      : Unsigned_16;
         Crumb_Class : Packet_Class;
      begin
         for I in Result'Range loop
            Read (Packet => Input,
                  Value  => Buffer,
                  N_Bits => 16);

            Crumb_Size := Positive(Buffer and 2#0011_1111_1111_1111#);

            if (Buffer and 2#0100_0000_0000_0000# = 0) then
               Crumb_Class := Analog;
            else
               Crumb_Class := Binary;
            end if;

            Result(I) := (Timestamp => Timestamp,
                          Class     => Crumb_Class,
                          Payload   =>
                            new Stream_Element_Array(1..Crumb_Size));

            Read (Packet => Input,
                  Data   => Result(I).Payload);
         end loop;

         return Result;
      end;
   end Split;

end Merging;
