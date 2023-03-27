with Byte_Arrays;            use Byte_Arrays;
with PPETP;                  use PPETP;
with Ada.Text_Io;                   use Ada.Text_Io;
-- with Host_To_Network, PPETP;
-- use  Host_To_Network, PPETP;

package body Packets.Protocol.Utilities is
   use type Byte_Arrays.Byte;
--     ------------------
--     -- To_Timestamp --
--     ------------------
--
--     -- Reconstruct a timestamp from its "high" and "low"
--     -- part
--
--     function To_Timestamp (Data : Profile_Timestamp_Buffer)
--                            return Timestamp_Type is
--     begin
--        -- Put_Line (Byte'Image (Data (1)) & Byte'Image(Data(1) and 15));
--        -- Put_Line (Byte'Image (Data (2)));
--        -- Put_Line (Byte'Image (Data (3)));
--        return Timestamp_Type (Data (1) and 16#0F#) +
--          (2 ** 4)  * Timestamp_Type (Data (2)) +
--          (2 ** 12) * Timestamp_Type (Data (3));
--     end To_Timestamp;

   ----------------
   -- To_Channel --
   ----------------
--     function To_Channel (Data : Profile_Timestamp_Buffer)
--                          return PPETP_Channel_ID is
--     begin
--
--
--        return PPETP_Channel_ID(Get_Flags(Data));
--     end To_Channel;


--     ---------------
--     -- To_Buffer --
--     ---------------
--
--     procedure To_Buffer (Buffer    : in out Profile_Timestamp_Buffer;
--                          Timestamp : in     PPETP.Timestamp_Type) is
--     begin
--        Buffer (1) := (Buffer (1) and 16#F0#) or Byte (Timestamp and 16#0_00_0F#);
--        Buffer (2) := Byte ((Timestamp and 16#0_0F_F0#) / 2 ** 4);
--        Buffer (3) := Byte (Timestamp / 2 ** 12);
--     end To_Buffer;

--     procedure Set_Flags (Buffer : in out Profile_Timestamp_Buffer;
--                          Flags  : in     Bit_Field_4) is
--     begin
--        Buffer (1) := (Buffer (1) and 16#0F#) or (Byte (Flags) * 2 ** 4);
--     end Set_Flags;

--     procedure To_Buffer (Buffer    : in out Profile_Timestamp_Buffer;
--                          Timestamp : in     PPETP.Timestamp_Type;
--                          Flags     : in     Bit_Field_4) is
--     begin
--        Set_Flags(Buffer, Flags);
--        To_Buffer(Buffer, Timestamp);
--     end To_Buffer;



--     function Get_Flags (Buffer : Profile_Timestamp_Buffer) return Bit_Field_4 is
--     begin
--        return Bit_Field_4(Buffer(1) / 2**4);
--     end Get_Flags;

--     procedure Parse_Profile_Timestamp_Buffer (Buffer    : in     Profile_Timestamp_Buffer;
--                                               Timestamp :    out PPETP.Timestamp_Type;
--                                               Flags     :    out Bit_Field_4) is
--     begin
--        Timestamp := To_Timestamp(Buffer);
--        Flags     := Get_Flags(Buffer);
--     end Parse_Profile_Timestamp_Buffer;


   function Get_Flags_From_Profile(Profile: Profile_Type) return Bit_Field_4 is
   begin
      return Profile_Type'Pos(Profile);
   end Get_Flags_From_Profile;

   --     ------------
   --     -- Get_Hi --
   --     ------------
   --
   --     function Get_Hi (T : PPETP.Timestamp_Type)
   --                     return bit_field_4 is
   --     begin
   --        return bit_field_4 (T / 2 ** 16);
   --     end Get_Hi;
   --
   --
   --     ------------
   --     -- Get_Lo --
   --     ------------
   --
   --     function Get_Lo (T : PPETP.Timestamp_Type)
   --                     return bit_field_16 is
   --     begin
   --        return Host_To_Network_16(Unsigned_16(T mod 2**16));
   --     end Get_Lo;


   -----------
   -- "xor" --
   -----------

   function "xor" (A, B : Network_Utilities.Inet_Addr_VN_Buffer)
                   return Network_Utilities.Inet_Addr_VN_Buffer is
      Result : Network_Utilities.Inet_Addr_VN_Buffer (A'Range);
   begin
      pragma Assert (Check => A'First = B'First and A'Last = B'Last);
      for I in Result'Range loop
         Result (I) := A (I) xor B (I);
      end loop;

      return Result;
   end "xor";
end Packets.Protocol.Utilities;
