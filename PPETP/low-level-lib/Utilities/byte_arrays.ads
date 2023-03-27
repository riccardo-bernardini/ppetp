with Ada.Streams;               use Ada.Streams;
with Ada.Unchecked_Deallocation;

package byte_arrays is
   subtype Byte              is Stream_Element;

   subtype Byte_Array_Offset is Stream_Element_Offset;

   type Byte_Array        is
     array (Byte_Array_Offset range <>) of Byte;

   type Byte_Array_Pt is access Byte_Array;

   function New_Byte_Array (X : Stream_Element_Array)
                            return Byte_Array_Pt;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Byte_Array,
                                     Name   => Byte_Array_Pt);

   procedure Dump (X : Byte_Array; S : String := "");

   function Dump (X : Byte_Array; C : Boolean := False) return String;
   --function Dump (X : Byte_Array) return String;
end byte_arrays;
