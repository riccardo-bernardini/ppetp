with Ada.Unchecked_Deallocation;
with Ada.Numerics.Discrete_Random;

with Interfaces;
use  Interfaces;

with Text_Io;

package Bytes is
   type Byte is new Unsigned_8;

   type Byte_Array is
     array(Positive range <>) of Byte;

   type Byte_2D_Array is
     array(Positive range <>, Positive range <>) of Byte;

   type Byte_Array_Pt is access Byte_Array;

   procedure Delete_Byte_Array is
      new Ada.Unchecked_Deallocation(Byte_Array, Byte_Array_Pt);

   package Random_Byte_Pkg is
      new Ada.Numerics.Discrete_Random(Byte);

   subtype Byte_Generator is Random_Byte_Pkg.Generator;

   function Random_Byte(Gen: Byte_Generator) return Byte
     renames Random_Byte_Pkg.Random;

   package Byte_IO is new Text_Io.Integer_Io(Integer);
end  Bytes;
