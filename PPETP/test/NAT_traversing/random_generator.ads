--                              -*- Mode: Ada -*-
--  Filename        : server_main.ads
--  Description     : Random Number Generator
--  Author          : Roberto Cesco Fabbro
--  Created On      : Gen, 27 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          :

-- This Package provide a random number generator implementing the generic package
-- Ada.Numerics.Discrete_Random

with Random_Integer; use Random_Integer;
with Interfaces; use Interfaces;

package Random_Generator is

   -- Return an Integer random number, the number is between -max..max
   function Random_Int(max: Positive) return Integer;

   -- Return an Unsigned_32 random number,
   function Random_Uns_32 return Unsigned_32;

   -- Return a Natural random number, the number is between 0..max
   function Random_Nat(max: Positive) return Natural;

   -- Return a Natural random number, the number is between 1..max
   function Random_Pos(max: Positive) return Positive;

private

   G: Generator;

end Random_Generator;





