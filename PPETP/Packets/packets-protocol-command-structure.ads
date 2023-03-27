--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-command-structure.ads
--  Description     : Bit-level structure of a command packet
--  Author          : Riccardo Bernardini
--  Created On      : Wed Nov  5 09:52:24 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with System;
with Packets.Protocol.Utilities;    use Packets.Protocol.Utilities;
with Network_Utilities;

private package Packets.Protocol.Command.Structure is
   -- ======================== --
   -- == Send/Receive Flags == --
   -- ======================== --

   type Reserved_Field   is new Natural range 0 .. 2**4-1;
   type Reserved_Field_1 is new Natural range 0 .. 2**4-1;
   type RH_Length_Type   is new Natural range 0 .. 2**8-1;

   WORD : constant := 4;	-- the size in byte of a word



   -- ==================== --
   -- == Command header == --
   -- ==================== --

   type PPETP_Command_Header is
      record
         Version        : PPETP.Version_Type;
         Command        : Boolean;
         Padding	: Boolean;
         Request        : Request_Type;
	 Flags		: Request_Flags;
         RH_Length	: RH_Length_Type;
         PPETP_Magic	: PPETP.PPETP_Magic;
         Sequence_Num	: PPETP.Command_Sequence_Number;
         Sub_Seq_Num    : PPETP.Sub_Sequence_Number;
      end record;

   pragma Warnings (Off);
   for PPETP_Command_Header'Bit_Order use System.High_Order_First;
   for PPETP_Command_Header'Size use 9*8;
   for PPETP_Command_Header use
      record
         Version        at 0 * WORD range 0  .. 1;
         Command        at 0 * WORD range 2  .. 2;
         Padding	at 0 * WORD range 3  .. 3;
         Request        at 0 * WORD range 4  .. 7;
         Flags		at 0 * WORD range 8  .. 15;
         RH_Length	at 0 * WORD range 16 .. 23;
         PPETP_Magic	at 0 * WORD range 24 .. 31;
         Sequence_Num	at 1 * WORD range 0 .. 31;
         Sub_Seq_Num    at 2 * WORD range 0 .. 7;
      end record;
   pragma Warnings (On);

   PPETP_Command_Header_Size : constant Natural := PPETP_Command_Header'Size / Byte'Size;


   subtype PPETP_Command_Header_Buffer is
     Byte_Array(1 .. Byte_Array_Offset(PPETP_Command_Header_Size));



end Packets.Protocol.Command.Structure;
