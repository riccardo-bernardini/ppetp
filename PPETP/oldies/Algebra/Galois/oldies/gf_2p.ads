--                              -*- Mode: Ada -*-
--  Filename        : gf_2p.ads
--  Description     : Generic package for GF(2^n), n <= 64
--  Author          : Riccardo Bernardini
--  Created On      : Thu Nov 15 17:01:13 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED> for Exp in 1..32
--

--
-- Typical usage
--
--    package GF256 is new GF_2p(8);
--    use GF256;
--
--    A, B : Galois;
--
with Ada.Streams, Interfaces;
use  Ada.Streams, Interfaces;

generic
   Exponent : Positive;
   -- To work with GF(2^p) use Exponent=p

   Small_Footprint : Boolean := False;
   -- If Small_Footprint is True, use algorithms which are slower, but
   -- require less memory.  If Small_Footprint is False, try to use
   -- the faster algorithm (but fall back to the slower ones if
   -- Exponent > 16)

package Gf_2p is
   type Galois is private;

   type Galois_Array is array (Positive range <>) of Galois;

   function To_Int(X: Galois) return Interfaces.Unsigned_64;
   function To_Galois(X: Interfaces.Unsigned_64) return Galois;
   function To_Galois(X: Integer) return Galois;

   function Galois_Array_To_Stream(X : Galois_Array)
                                  return Stream_Element_Array;

   function Stream_To_Galois_Array (X : Stream_Element_Array)
                                   return Galois_Array;

   Zero : constant Galois;
   One  : constant Galois;

   Size : constant Interfaces.Unsigned_32;
   Name : constant String;

   function "+"(Left, Right: Galois) return Galois;
   function "-"(Left, Right: Galois) return Galois;
   function "-"(X: Galois) return Galois;

   function "*"(Left, Right: Galois) return Galois;
   function "/"(Num, Den: Galois) return Galois;
   function Inv(X: Galois) return Galois;

   function Image(X: Galois) return String;
   -- Equivalent to Integer'Image(To_Int(X))

   function Is_Unit(X: Galois) return Boolean;
   -- Return True if X is non-zero
private
   type Galois is new Interfaces.Unsigned_64;

   Zero : constant Galois := 0;
   One  : constant Galois := 1;

   Name : constant String   := "GF(2^" & Integer'Image(exponent) & ")";
   Size : constant Interfaces.Unsigned_32 := 2**Exponent;
end Gf_2p;
