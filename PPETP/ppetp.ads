--                              -*- Mode: Ada -*-
--  Filename        : ppetp.ads
--  Description     : Root PPETP package
--  Author          :
--  Created On      : Sat Jul 19 12:13:44 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package contains the definitions which are needed both by
-- the external interface and the internal code.
--

with Network;
with Ada.Containers;    use Ada.Containers;
with Interfaces;	use Interfaces;
with Unchecked_Deallocation;

package PPETP is
   type Session_ID is new Natural range 0 .. 2**16-1;
   type Channel_ID is new Natural range 0 .. 15;
   type Target_ID  is new Natural;
   type PPETP_Magic is new Natural range 0 .. 2**8-1;

   type Stream_ID   is new Natural range 0 .. 2**12-1;
   No_Stream_ID : constant Stream_ID;

   type Data_Sequence_Number is mod 2**20;
   type Command_Sequence_Number is mod 2**32;
   type Sub_Sequence_Number is mod 2**8;

   PPETP_Magic_Default : constant PPETP_Magic := 95;



   subtype PPETP_Channel_ID is PPETP.Channel_ID;   -- Channel ID at PPETP level

   ---------------------
   -- Peer Identifier --
   ---------------------
   type Peer_ID  is  new Unsigned_32 range 0 .. 2**32-1;
   No_Peer_ID : constant Peer_ID;


   --function Hash(Key : Peer_ID) return Hash_Type;
   --function Equivalent_Keys(Left, Right : Peer_ID) return boolean;
   function "=" (Left, Right : Peer_ID) return boolean;
   function "<" (Left, Right : Peer_ID) return boolean;
   function Value(Str: String) return Peer_ID;

   type Peer_ID_Access is access Peer_ID;

   procedure Free is new Unchecked_Deallocation (Peer_ID, Peer_ID_Access);

   -----------------------------
   -- Basic PPETP definitions --
   -----------------------------

   -- PPETP Timestamp
   --Timestamp_Bitlen : constant Natural := 20;
   --type Timestamp_Type is mod 2**Timestamp_Bitlen;

   type Version_Type is mod 2**2;

   Protocol_Version : constant Version_Type := 0;

   -----------------------------------------------
   -- Vandermonde reduction profile definitions --
   -----------------------------------------------

   -- Galois Field used for packet reduction
   type GF_Name is  (GF_8, GF_16, GF_24, GF_32);
   for  GF_Name use
     (GF_8 => 0, GF_16 => 1, GF_24 => 2, GF_32 => 3);

   type RV_Index  is mod 2**32;

   Reduction_Factor_Size : constant Natural := 8;
   type Reduction_Factor_Type is mod 2**Reduction_Factor_Size;
private
   No_Peer_ID : constant Peer_ID := 0;
   No_Stream_ID : constant Stream_ID := 0;
end PPETP;
