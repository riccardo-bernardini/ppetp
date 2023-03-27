with Ada.Streams;     use Ada.Streams;
with Byte_Arrays;     use Byte_Arrays;

package  Profiles.Entangled.Basic is
   type Basic_Ent is
     new Root_Entangled_Payload with
      record
         Bin_Data : Byte_Array_Pt;
      end record;

   type Basic_Ent_Pt is access all Basic_Ent;

   function "=" (X, Y: Basic_Ent) return Boolean;

end Profiles.Entangled.Basic;
