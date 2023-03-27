with Interfaces;                    use Interfaces;
with Packets.Protocol.Utilities;    use Packets.Protocol.Utilities;
with Byte_Arrays;                   use Byte_Arrays;

with System;

private package Packets.Protocol.Data.Structure is

   type Reserved_Field is new Natural range 0 .. 2**12-1;

   WORD : constant := 4;	-- the size in byte of a word

   -------------------------------------------------------
   -- Definition and representation of the PPETP header --
   -------------------------------------------------------

   type PPETP_Data_Header is
      record
         Version	: PPETP.Version_Type;  -- Word 0
         Command        : Boolean;
         Padding        : Boolean;
         Inline         : Boolean;
         Prof_Flags     : Profiles.Profile_Flags;
         Channel	: PPETP.Channel_ID;
         Reserved	: Reserved_Field;
 	 Ppetp_Magic	: PPETP.PPETP_Magic;
         Stream_ID	: PPETP.Stream_ID;	-- Word 1
         Sequence_Num	: PPETP.Data_Sequence_Number;
      end record;


   pragma Warnings (Off);
   for PPETP_Data_Header'Bit_Order use System.Low_Order_First;
   for PPETP_Data_Header use
      record
         Version	at 0 * WORD range 0  .. 1;
         Command        at 0 * WORD range 2  .. 2;
         Padding        at 0 * WORD range 3  .. 3;
         Inline         at 0 * WORD range 4  .. 4;
         Prof_Flags     at 0 * WORD range 5  .. 7;
         Channel        at 0 * WORD range 8  .. 11;
         Reserved      	at 0 * WORD range 12 .. 23;
         Ppetp_Magic    at 0 * WORD range 24 .. 31;
         Stream_ID	at 1 * WORD range 0  .. 11;
         Sequence_Num	at 1 * WORD range 12 .. 31;
      end record;
   pragma Warnings (On);

   PPETP_Data_Header_Size : constant Natural :=
     PPETP_Data_Header'Size / Byte'Size;

   subtype PPETP_Data_Header_Buffer is
     Byte_Array(1 .. Byte_Array_Offset(PPETP_Data_Header_Size));


end Packets.Protocol.Data.Structure;
