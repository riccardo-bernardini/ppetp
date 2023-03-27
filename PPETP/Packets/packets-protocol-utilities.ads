--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-utilities.ads
--  Description     : Utilities for parsing/building protocol packets
--  Author          : Riccardo Bernardini
--  Created On      : Mon Nov  3 15:34:07 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <Tested>

--
-- This package provides few utilities for parsing and building
-- protocol packets.
--

with Interfaces;                   use  Interfaces;
with Byte_Arrays;                  use Byte_Arrays;

with Network_Utilities;
with PPETP;			use PPETP;
with Profiles;			use Profiles;


--private
package Packets.Protocol.Utilities is
--   subtype Profile_Timestamp_Buffer is Byte_Array (1 .. 3);

   type Bit_Field_4 is mod 2 ** 4;
   for  Bit_Field_4'Size use 4;

--     type bit_field_8 is mod 2 ** 8;
--     for  bit_field_8'Size use 8;
--
--     type bit_field_16 is mod 2 ** 16;
--     for  bit_field_16'Size use 16;
--
--     type bit_field_32 is mod 2 ** 32;
--     for  bit_field_32'Size use 32;

   subtype Bit_Field_8  is Interfaces.Unsigned_8;
   subtype Bit_Field_16 is Interfaces.Unsigned_16;
   subtype Bit_Field_32 is Interfaces.Unsigned_32;

   -- Reconstruct a timestamp from its "high" and "low"
   -- part
--     function To_Timestamp (Data : Profile_Timestamp_Buffer)
--                            return PPETP.Timestamp_Type;

--     function To_Channel (Data : Profile_Timestamp_Buffer)
--                          return PPETP_Channel_ID;

   -- Write Timestamp in Buffer, according to the PPETP representation.
   -- Leaves the 4 most significant bits of Buffer(1) unchanged.
--     procedure To_Buffer (Buffer    : in out Profile_Timestamp_Buffer;
--                          Timestamp : in     PPETP.Timestamp_Type);

--     procedure To_Buffer (Buffer    : in out Profile_Timestamp_Buffer;
--                          Timestamp : in     PPETP.Timestamp_Type;
--                          Flags     : in     Bit_Field_4);

--     procedure Set_Flags (Buffer : in out Profile_Timestamp_Buffer;
--                          Flags  : in     Bit_Field_4);
--
--     function Get_Flags (Buffer : Profile_Timestamp_Buffer) return Bit_Field_4;

   function Get_Flags_From_Profile(Profile: Profile_Type) return Bit_Field_4;

--     procedure Parse_Profile_Timestamp_Buffer (Buffer    : in     Profile_Timestamp_Buffer;
--                                       Timestamp :    out PPETP.Timestamp_Type;
--                                       Flags     :    out Bit_Field_4);


   function "xor" (A, B : Network_Utilities.Inet_Addr_VN_Buffer)
                  return Network_Utilities.Inet_Addr_VN_Buffer;
end Packets.Protocol.Utilities;
