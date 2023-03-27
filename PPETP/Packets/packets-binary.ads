--                              -*- Mode: Ada -*-
--  Filename        : packets-binary.ads
--  Description     : Definition of the root of binary packets
--  Author          : Finta Tartaruga
--  Created On      : Tue Nov  4 21:48:42 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <FULLY TESTED>. (Nov  4 21:48:42 2008)

with Ada.Streams;         use Ada.Streams;
with Ada.Finalization;    use Ada.Finalization;
with Byte_Arrays;         use Byte_Arrays;

package Packets.Binary is
   type Binary_Packet is abstract new Controlled with private;

   function Get (Packet : Binary_Packet;
                 Index  : Byte_Array_Offset)
                return Byte;

   function Buffer (Packet : Binary_Packet)
                   return Byte_Array;


   procedure Reserve (Packet : in out Binary_Packet;
                      Length : in     Byte_Array_Offset);

   procedure Set (Packet : in out Binary_Packet;
                  Data   : in     Byte_Array);

   procedure Set (Packet : in out Binary_Packet;
                  Index  : in     Byte_Array_Offset;
                  Data   : in     Byte);
private
   -- type Stream_Element_Array_Pt is access Stream_Element_Array;

   type Binary_Packet is
     new Controlled with
      record
         Data_Buffer : Byte_Array_Pt := null;
      end record;

   procedure Adjust     (Object : in out Binary_Packet);
   procedure Finalize   (Object : in out Binary_Packet);
end Packets.Binary;
