

package body Random_Generator is

   function Random_Int(max: Positive) return Integer is
   begin
      -- it uses the REM instead of MOD because MOD doesn't give negative values
      return Integer((Random(G) rem max) + 1);
   end Random_Int;


   function Random_Uns_32 return Unsigned_32 is
      tmp: long_long_integer;
   begin
      tmp := abs(long_long_integer(Integer'First)) + long_long_integer(Random(G));

      return Unsigned_32(tmp);
   end Random_Uns_32;


   function Random_Nat(max: Positive) return Natural is
   begin
      return Natural( (abs(Random(G)) mod max) + 1 );
   end Random_Nat;


   function Random_Pos(max: Positive) return Positive is
      tmp: Integer :=0;
   begin

      -- if the random function generate a 0 then loop again
      while tmp = 0 loop

         tmp := Random(G);
      end loop;

      return Positive( (abs(Random(G)) mod max) + 1 );

   end Random_Pos;



begin

   Reset(G); -- Initialize the random generator

end Random_Generator;

