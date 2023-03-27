--                              -*- Mode: Ada -*-
--  Filename        : parsing_utilities.ads
--  Description     : Mixed utilities for packet parsing
--  Author          : Riccardo Bernardini
--  Created On      : Wed Sep 24 13:10:44 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <FULLY TESTED>  (2008-10-23 14:27)

--
-- This package provides a type Parsing_Buffer (and associated
-- methods) which is useful in parsing received packets.  More
-- or less, a Parsing_Buffer is similar to a file initialized
-- with the bytes of the received packet.
--

with Ada.Streams;        use Ada.Streams;
with Ada.Finalization;   use Ada.Finalization;
with Smart_Records;      use Smart_Records;
with Byte_Arrays;        use Byte_Arrays;

package Parsing_Buffers is
   -- Raised when the constraints on the number of the
   -- remaining samples is not satisfied.
   Length_Constraint : exception;

   -- A Parsing Buffer is a convenient way to keep the data
   -- received from the network.  It is possible to extract
   -- one byte at time and it keeps track of the last read
   -- byte.
   type Parsing_Buffer is
     new Limited_Controlled with private;

   -- Make a parsing buffer out of a Raw_Packet
   function Make_Parsing_Buffer (Packet : byte_Array)
                                return Parsing_Buffer;

   procedure Fill (Src : in out Parsing_Buffer;
                   Dst :    out byte_Array);

   -- Convert a suitable number of bytes into a value
   -- of type Target ('a la' Unchecked_Conversion).
   generic
      type Target is private;
   procedure Extract (Packet : in out Parsing_Buffer;
                      Result :    out Target);

   -- Copy what remains in Buffer into the Raw_Packet Data
   generic
      type Target is access Byte_Array;
   procedure Get_Remaining (Buffer : in out Parsing_Buffer;
                            Data   :    out Target);

   -- Delete the last How_Many elements from Buffer.  Raises
   -- Invalid_Packet if there are not enough bytes in Buffer.
   procedure Remove_Last (Buffer   : in out Parsing_Buffer;
                          How_Many : in     Byte_Array_Offset);

   -- Return the value of the Pos'th element of Buffer
   function Data (Buffer : Parsing_Buffer;
                  Pos    : Byte_Array_Offset)
                  return Byte;

   -- Returns the current byte pointed by cursor --
   -- NOTE: this function has not been tested yet --
   function Get_Actual (Buffer : Parsing_Buffer)
                  return Byte;

   -- Return the index of the last element of Buffer
   -- (somehow similar to the 'Last attribute for arrays)
   function Last (Buffer : Parsing_Buffer)
                  return Byte_Array_Offset;

    -- Return the index of the first element of Buffer
   function Actual_Cursor (Buffer : Parsing_Buffer)
                           return Byte_Array_Offset;

   procedure Set_Last (Buffer : in out Parsing_Buffer;
                       Last   : in     Byte_Array_Offset);

   procedure Set_Actual (Buffer : in out Parsing_Buffer;
                         Actual : in     Byte_Array_Offset);

   function Remaining (Buffer : Parsing_Buffer)
                  return Byte_Array_Offset;

   -- Skip the next How_Many entries of Buffer
   procedure Skip (Buffer   : in out Parsing_Buffer;
                   How_Many : in     Byte_Array_Offset);

   -- Three procedures to check the number of remaining
   -- bytes in a Parsing_Buffer.  "Die" means "raise Length_Constraint"
   procedure Remain_Exactly_Or_Die (Where    : Parsing_Buffer;
                                    How_Many : Byte_Array_Offset);

   procedure Remain_At_Least_Or_Die (Where    : Parsing_Buffer;
                                     How_Many : Byte_Array_Offset);

   procedure Die_If_Not_Empty (What : Parsing_Buffer);
private
   type Parsing_Buffer is
     new Limited_Controlled with
      record
         Data   : Byte_Array_Pt;
         Cursor : Byte_Array_Offset;
         Last   : Byte_Array_Offset;
      end record;

   overriding
   procedure Finalize (X : in out Parsing_Buffer);
end Parsing_Buffers;

