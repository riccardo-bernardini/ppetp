-- Status: Basic test OK
package Str_Conversion is
   Invalid_Number : exception;

   type Error_Policy is (Die, Stop_Parsing);

   subtype Base_Type is Integer range 2 .. 16;

   generic
      type Int is range <>;
   function Integer_To_String (X    : Int;
                               Base : Base_Type)
                               return String;

   generic
      type Int is mod <>;
   function Modular_To_String (X    : Int;
                               Base : Base_Type;
                               Len  : Natural := 0)
                               return String;

   -- Interpret X as a number in base Base and convert it to
   -- integer.  On_Error specifies what to do if any invalid
   -- char is found after a valid "head" (e.g., if X is "123xyz",
   -- "x" is not a valid digit, but "123" is a valid number).
   -- If On_Error = Die, Invalid_Number is raised, otherwise
   -- the value relative to the valid head is returned.  Note
   -- that if no valid head is present (e.g. X = "xyz" or
   -- X = "-"), Invalid_Number is always raised, indepenedently
   -- on the value of On_Error.
   generic
      type Int is range <>;
   function String_To_Integer (X        : String;
                               Base     : Base_Type;
                               On_Error : Error_Policy := Die)
                              return Int;

   -- Same as String_To_Integer, but for modular types
   generic
      type Int is mod <>;
   function String_To_Modular (X        : String;
                               Base     : Base_Type;
                               On_Error : Error_Policy := Die)
                               return Int;
end Str_Conversion;
